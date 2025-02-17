import typing
import itertools
import random
import datetime
import pandas as pd

## validation information
nfl_names = pd.read_csv('https://gist.githubusercontent.com/cnizzardini/13d0a072adb35a0d5817/raw/f315c97c7677845668a9c26e9093d0d550533b00/nfl_teams.csv')
afc_names_set = set(nfl_names[nfl_names['Conference'] == 'AFC']['Abbreviation'])
nfc_names_set = set(nfl_names[nfl_names['Conference'] == 'NFC']['Abbreviation'])

def validate_team_abbr(conference, names_set):
    team_name = input(f'Input the team abbreviation for the {conference}: ').upper()
    if team_name not in names_set:
        raise ValueError(f'Invalid {conference} team abbreviation, terminating program.')
    return team_name

def get_new_name(player_dict:typing.Dict[None, None]) -> str:
    name_stop = False
    while not name_stop:
        print("Current player/box number count:")
        if player_dict:
            for k,v in player_dict.items():
                print(f"\tPlayer {k} has selected {v} boxes.")
        new_name = input("Please enter the player's name: ")
        if new_name not in player_dict:
            name_stop = True
        else:
            print(f'{new_name} already exists, please choose an unique name for the player.')
    return new_name

def get_box_count(player_dict:typing.Dict[None, None]) -> int:
    box_stop = False
    while not box_stop:
        try:
            remaining_boxes = 100 - sum(player_dict.values())
            number_boxes = int(input('Number of remaining boxes: {}\nPlease enter the number of boxes for the player: '.format(remaining_boxes)))
            if number_boxes < 1:
                print('Please input a positive integer value.')
                continue
            if remaining_boxes - number_boxes < 0:
                print(f'Number of boxes exceeds grid amount, please input a positive integer value less than or equal to {remaining_boxes}.')
                continue
            else:
                box_stop = True
        except ValueError:
            print('Invalid number, please input a positive integer value for the number of boxes.')
    return number_boxes

def keep_prompting(player_dict:typing.Dict[None, None]) -> bool:
    if sum(player_dict.values()) == 100:
        return False
    keep_playing_prompt = input('Add another player? Input "no" to cancel: ').lower()
    if keep_playing_prompt == 'no':
        return False
    return True

def get_players_and_box_counts() -> dict:
    continue_to_prompt = True
    player_dict = {}
    while continue_to_prompt:
        player_name = get_new_name(player_dict)
        number_boxes = get_box_count(player_dict)
        player_dict[player_name] = number_boxes
        continue_to_prompt = keep_prompting(player_dict)
    return player_dict

def create_random_players_list(player_dict:typing.Dict[str, int]) -> typing.List[str]:
    player_boxes = [player for player, box_count in player_dict.items() for _ in range(box_count)]
    empty_boxes = ['EMPTY'] * (100 - len(player_boxes))
    players_list = player_boxes + empty_boxes
    for _ in range(10):
        random.shuffle(players_list)
    return players_list

def generate_player_grid(players_list:typing.List[str], AFC_team:str, NFC_team:str) -> pd.DataFrame:
    grid_ind = list(itertools.product(range(10), range(10)))
    grid_dict = {AFC_team : [f'{AFC_team}:{i[0]}' for i in grid_ind], NFC_team : [f'{NFC_team}:{i[1]}' for i in grid_ind], 'players' : players_list}
    return pd.DataFrame(grid_dict).pivot(index=AFC_team, columns=NFC_team, values='players')

def int_to_roman(number):
    ROMAN = {
        'M' : 1000,
        'CM' : 900,
        'D' : 500,
        'CD' : 400,
        'C' : 100,
        'XC' : 90,
        'L' : 50,
        'XL' : 40,
        'X' : 10,
        'IX' : 9,
        'V' : 5,
        'IV' : 4,
        'I' : 1,
    }
    result = []
    for (roman, arabic) in ROMAN.items():
        factor, number = divmod(number, arabic)
        result.append(roman * factor)
        if number == 0:
            break
    return "".join(result)

def int_to_roman(number):
    ROMAN = {
        'M' : 1000,
        'CM' : 900,
        'D' : 500,
        'CD' : 400,
        'C' : 100,
        'XC' : 90,
        'L' : 50,
        'XL' : 40,
        'X' : 10,
        'IX' : 9,
        'V' : 5,
        'IV' : 4,
        'I' : 1,
    }
    result = []
    for (roman, arabic) in ROMAN.items():
        factor, number = divmod(number, arabic)
        result.append(roman * factor)
        if number == 0:
            break
    return "".join(result)

def roman_to_int(s):
    ROMAN = {
        'M' : 1000,
        'CM' : 900,
        'D' : 500,
        'CD' : 400,
        'C' : 100,
        'XC' : 90,
        'L' : 50,
        'XL' : 40,
        'X' : 10,
        'IX' : 9,
        'V' : 5,
        'IV' : 4,
        'I' : 1,
    }
    i = 0
    num = 0
    while i < len(s):
        if i + 1 < len(s) and s[i:(i+2)] in ROMAN:
            num += ROMAN[s[i:(i+2)]]
            i += 2
        else:
            num += ROMAN[s[i]]
            i += 1
    return num

def create_output_file_name(AFC_team, NFC_team):
    super_bowl_number = datetime.datetime.now().year - 1966
    super_bowl_numeral = int_to_roman(super_bowl_number)
    return f'SB {super_bowl_number} {AFC_team} vs {NFC_team} random grid.csv'

def get_super_bowl_grid() -> pd.DataFrame:
    print('Welcome to the Super Bowl random box generator!\n\n')
    AFC_team = validate_team_abbr('AFC', afc_names_set)
    NFC_team = validate_team_abbr('NFC', nfc_names_set)
    player_dict = get_players_and_box_counts()
    players_list = create_random_players_list(player_dict)
    player_grid = generate_player_grid(players_list, AFC_team, NFC_team)
    output_file_name = create_output_file_name(AFC_team, NFC_team)
    player_grid.to_csv(output_file_name)
    print(f'Successfully wrote {output_file_name} to a .csv file.')