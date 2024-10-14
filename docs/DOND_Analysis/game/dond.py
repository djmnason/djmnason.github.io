from dond_utils import *
import os, time

# # Start the game
def play_dond():
    os.system('cls')
    print("Welcome!\n\nLet's play Deal or No Deal!")
    case_values = [
        0.01, 1.0, 5.0, 10.0, 25.0, 50.0, 75.0, 100.0,
        200.0, 300.0, 400.0, 500.0, 750.0, 1000.0, 5000.0,
        10000.0, 25000.0, 50000.0, 75000.0, 100000.0,
        200000.0, 300000.0, 400000.0, 500000.0, 750000.0, 1000000.0,
    ]
    random.shuffle(case_values)
    cases = {i+1 : case_values[i] for i in range(len(case_values))}
    deal_cases = cases.copy()

    turns = [6,5,4,3,2,1,1,1,1]
    round_offer_accepted = None
    stay = None
    other_choice = None
    offers_tracker = {}

    player_case_number = select_valid_case(cases=cases)
    print(f'You chose case number: {player_case_number}')
    player_case_value = cases.pop(player_case_number)

    for current_round, round_number in enumerate(range(len(turns)), start=1):
        print_round_start(current_round, cases, case_values)
        for _ in range(turns[round_number]):
            current_turn_case = select_valid_case(cases)
            eliminated_value = cases.pop(current_turn_case)
            deal_cases.pop(current_turn_case)
            case_values.pop(case_values.index(eliminated_value))
            print(f"Case #{current_turn_case} contained ${eliminated_value:.2f}")
        
        ## calculate offer
        current_offer = calculate_banker_offer(current_round, deal_cases)
        offers_tracker[current_round] = current_offer
        print(f"The banker's offer for round {current_round} is ${current_offer:.2f}.")
        time.sleep(1)

        ## if player doesn't have deal already
        if round_offer_accepted:
            pass

        else:
            ## ask player to take the offer
            player_decision = validate_player_decision(prompt="Deal or No Deal? (deal/no deal):", decisions=['no deal', 'deal'])
            
            if player_decision == 'deal':
                player_winnings = current_offer
                round_offer_accepted = current_round
                print('You took the deal!')
                if current_round < 9:
                    print("Let's see what would have happened if you had not accepted the deal.")
                    time.sleep(1)
        

        ## add what-if logic
        os.system('cls')
    else:
        if round_offer_accepted:
            pass
        else:
            stay = validate_player_decision(
                prompt='No more rounds left.\nRemaining values:\n{}\nStick with your original case? (y/n):'.format(', '.join(str(c) for c in sorted(case_values))),
                decisions=['y','n']
            )
            last_case = cases.pop(list(cases.keys())[0])
            if stay == 'y':
                player_winnings, other_choice = player_case_value, last_case
            else:
                player_winnings, other_choice = last_case, player_case_value
            offers_tracker[current_round + 1] = other_choice
    
    game_summary_message(
        player_winnings=player_winnings,
        offers_tracker=offers_tracker,
        player_case_number=player_case_number,
        player_case_value=player_case_value,
        stay=stay,
        other_choice=other_choice,
        round_offer_accepted=round_offer_accepted
    )

if __name__ == '__main__':
    play_dond()