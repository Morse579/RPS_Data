"""
Module for generating sample participant data to be used in rock, paper, scissors analysis.
Running the file as a script will simulate a single game with GAME_ROUNDS played between two players
and write the results of the game to a csv in the DATA_PATH dir.
"""
import argparse
import csv
import math
import random

DEBUG = False # set to True for helpful printing during data creation

# path for writing sample data files
DATA_PATH = "/data"
# headers for csv containing the results of each round
CSV_HEADERS = ["game_id", "round_index", "player1_id", "player2_id", \
    "player1_move", "player2_move", "player1_rt", "player2_rt", \
        "player1_outcome", "player2_outcome", "player1_points", "player2_points", \
            "player1_total", "player2_total"]

GAME_ROUNDS = 100 # number of simulated rounds per game
MAX_RT = 10000 # ms response time after which a "no_choice" outcome is selected
VALID_MOVES = ["rock", "paper", "scissors", "no_choice"] # valid moves in a round on which outcomes are based
VALID_OUTCOMES = ["win", "tie", "loss"] # valid outcomes of a round on which points are based
POINTS_LOOKUP = { # points allocation for each possible game outcome
    "win": 3,
    "tie": 1,
    "loss": -1
}

# Initialize command line flags
parser = argparse.ArgumentParser()
parser.add_argument('--verbose', help = 'Print helpful data', action = 'store_true')
ARGS = parser.parse_args()



# generic util to return a 16 digit unique identifier
def get_uuid():
    return str(math.floor(random.random() * 10000000000000000))



"""
Simple class definition for generating a new RPS player
"""
class RpsPlayer(object):
    def __init__(self, game_id):
        self.game_id = game_id # unique id for the game this player has been assigned to
        self.id = get_uuid() # unique id for this player
        self.total_points = 0 # cumulative points accrued by this player in the game

    """
    Increase player's total points based on the results of the most recent round
    """
    def update_points(self, outcome):
        if outcome in POINTS_LOOKUP:
            self.total_points = self.total_points + POINTS_LOOKUP[outcome]

    """
    Simulate a move for this player:
    currently just a random draw from rock, paper, scissors, no_choice
    """
    def get_move(self):
        return random.sample(VALID_MOVES, 1)[0]

    """
    Simulate a response time for this player's move selection:
    currently just a sample from a normal centered at 5000ms with sigma = 1000ms
    """
    def get_rt(self):
        return random.normalvariate(mu = 5000, sigma = 1000)

    """
    Generate the appropriate move and response time for this player: includes logic
    to make the sampled response time consistent with the move selection and actual behavior
    """
    def get_player_move(self):
        move = self.get_move()
        if move == "no_choice": # if simulated move is no_choice, player waited too long so rt is fixed
            rt = MAX_RT
        else:
            rt = self.get_rt()
            if rt >= MAX_RT:
                rt = MAX_RT - 1000 # arbitrary choice to get below maximum alloted time
            if rt < 250:
                rt = 1000 # arbitrary choice to avoid artificially low RTs
        return {"move": move, "rt": rt}


"""
Simple class definition for simulating an RPS round between two RpsPlayer entities
"""
class RpsRound(object):
    def __init__(self, game_id, player1, player2, round_index):
        self.game_id = game_id # unique id for the game this round is being played in
        self.player1 = player1 # RpsPlayer entity assigned to player1 in the game this round is being played in
        self.player2 = player2 # RpsPlayer entity assigned to player2 in the game this round is being played in
        self.round_index = round_index # index for this particular round in the game this round is being played in

    """
    Determine each player's points in a given round based on the outcomes of that round
    """
    def get_player_points(self):
        if self.player1_outcome in VALID_OUTCOMES:
            self.player1_points = POINTS_LOOKUP[self.player1_outcome]
            self.player1.update_points(self.player1_outcome)
        if self.player2_outcome in VALID_OUTCOMES:
            self.player2_points = POINTS_LOOKUP[self.player2_outcome]
            self.player2.update_points(self.player2_outcome)

    """
    Determine round outcome based on player moves
    """
    def get_player_outcomes(self):
        if self.player1_move in VALID_MOVES and self.player2_move in VALID_MOVES:
            # all possible tie outcomes
            if self.player1_move == self.player2_move:
                self.player1_outcome = self.player2_outcome = "tie"
            # player1 wins
            elif (self.player1_move == "rock" and self.player2_move == "scissors") or \
                (self.player1_move == "paper" and self.player2_move == "rock") or \
                    (self.player1_move == "scissors" and self.player2_move == "paper") or \
                        (self.player2_move == "no_choice"):
                self.player1_outcome = "win"
                self.player2_outcome = "loss"
            # all other: player2 wins
            else:
                self.player1_outcome = "loss"
                self.player2_outcome = "win"

    """
    Simulate this round by selecting moves for each player and determining results of those moves
    """
    def make_round(self):
        # Get each player's move
        player1_action = self.player1.get_player_move()
        player2_action = self.player2.get_player_move()
        # Assign round attrs based on results of each player's move
        self.player1_move = player1_action["move"]
        self.player1_rt = player1_action["rt"]
        self.player2_move = player2_action["move"]
        self.player2_rt = player2_action["rt"]
        # Assign outcomes for each player (win, tie, loss) based on each player's move
        self.get_player_outcomes()
        # Assign points for each player based on outcomes
        self.get_player_points()

    """
    Write the current state of this round to a csv
    """
    def write_to_file(self):
        filename = "TEST_{}.csv".format(self.game_id)
        with open(filename, mode = 'a') as f:
            writer = csv.writer(f, delimiter = ',')
            row = []
            for item in CSV_HEADERS:
                if item == "player1_id": # hacky
                    row.append(self.player1.id)
                elif item == "player1_total": # hacky
                    row.append(self.player1.total_points)
                elif item == "player2_id": # hacky
                    row.append(self.player2.id)
                elif item == "player2_total": # hacky
                    row.append(self.player2.total_points)
                else:
                    row.append(getattr(self, item))
            if ARGS.verbose:
                print("writing row to file: {}".format(row))
            writer.writerow(row)
        f.close()


    """
    DEBUGGING print the current state of this round
    """
    def print_round(self):
        print("RESULTS OF ROUND: {}".format(self.round_index))
        print("\t-------------")
        print("\tgame_id: {}".format(self.game_id))
        print("\tround_index: {}".format(self.round_index))
        print("\tplayer1_id: {}".format(self.player1.id))
        print("\tplayer2_id: {}".format(self.player2.id))
        print("\t------------")
        print("\tplayer1_move: {}".format(self.player1_move))
        print("\tplayer2_move: {}".format(self.player2_move))
        print("\tplayer1_rt: {}".format(self.player1_rt))
        print("\tplayer2_rt: {}".format(self.player2_rt))
        print("\t------------")
        print("\tplayer1_outcome: {}".format(self.player1_outcome))
        print("\tplayer2_outcome: {}".format(self.player2_outcome))
        print("\tplayer1_points: {}".format(self.player1_points))
        print("\tplayer2_points: {}".format(self.player2_points))
        print("\t------------")
        print("\tplayer1_total: {}".format(self.player1.total_points))
        print("\tplayer2_total: {}".format(self.player2.total_points))



def main():
    game_id = get_uuid()
    player1 = RpsPlayer(game_id)
    player2 = RpsPlayer(game_id)

    filename = "TEST_{}.csv".format(game_id)
    if ARGS.verbose:
        print("Writing results to: {}".format(filename))
    with open(filename, mode = 'w') as f:
        writer = csv.writer(f, delimiter = ',')
        writer.writerow(CSV_HEADERS)
    f.close()

    for round_index in range(1, GAME_ROUNDS + 1): # 1-index the game round
        rps_round = RpsRound(game_id, player1, player2, round_index)
        rps_round.make_round()
        if ARGS.verbose:
            rps_round.print_round()
        rps_round.write_to_file()


if __name__ == "__main__":
    main()
