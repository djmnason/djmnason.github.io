import pickle
import random

with open('dond_dict_model.pkl', 'rb') as f:
    model = pickle.load(f)

def select_valid_case(cases):
    while True:
        try:
            selection = int(input("Choose your case number (1-26): "))
            if selection in cases:
                return selection
        except:
            pass
        print('Invalid entry, input must be an available case number. Try again.')

def validate_player_decision(prompt, decisions):
    while True:
        player_decision = input(f"{prompt}: ").strip().lower()
        if player_decision in decisions:
            return player_decision
        print(f'Invalid input, please enter one of the following: {"/".join(decisions)}')

def deal_message_generator(
    player_winnings,
    offers_tracker,
    player_case_value,
    stay,
):
    ## player made a deal
    offers = list(offers_tracker.values())
    max_ = max(offers)
    if stay is None:
        if player_winnings > player_case_value:
            if player_winnings > max_:
                message = 'You made the best deal!'
            else:
                message = 'You made a good deal!'
        else:
            message = 'You did not make a good deal.'
    ## player made a decision at the end
    else:
        if stay == 'y':
            last = offers[-1]
            if player_winnings > last:
                if player_winnings > max_:
                    message = 'You made the best decision by keeping your case!'
                else:
                    message = 'You made the right decision by keeping your case!'
            else:
                message = 'You did not make the right decision by keeping your case.'
        else:
            if player_winnings > player_case_value:
                if player_winnings > max_:
                    message = 'You made the best decision by swapping your case!'
                else:
                    message = 'You made the right decision by swapping your case!'
            else:
                message = 'You did not make the right decision by swapping your case.'

    return message

## final message summary
def game_summary_message(
        player_winnings, 
        offers_tracker, 
        player_case_number,
        player_case_value, 
        stay,
        other_choice,
        round_offer_accepted=None
):
    deal_message = deal_message_generator(player_winnings, offers_tracker, player_case_value, stay)
    round_offer_accepted = round_offer_accepted or 10
    print('Game recap:\n')
    if stay is None:
        print(f"Your case {player_case_number} had a value of ${player_case_value:.2f}")
    else:
        if stay == 'y':
            print(f"Your case had a value of ${player_case_value:.2f} while the other case had a value of ${other_choice:.2f}.")
        else:
            print(f"The case you switched to had a value of ${player_winnings:.2f} while your original case had a value of ${player_case_value:.2f}.")
    for round_, offer in offers_tracker.items():
        if round_ < 10:
            if round_ == round_offer_accepted:
                accepted_message = 'Yes'
            elif round_ > round_offer_accepted:
                accepted_message = 'Already accepted'
            else:
                accepted_message = 'No'
            print(f"Round {round_} offer of ${offer:.2f}. Offer accepted?: {accepted_message}")
    
    print(f'{deal_message}\nYour winnings: ${player_winnings:.2f}\n\nThank you for playing Deal or No Deal!')

def print_round_start(round_number, cases, case_values):
    print(f'Round number {round_number}\n')
    print('Available cases:\n{}'.format(', '.join(str(c) for c in cases)))
    print('Values remaining\n{}'.format(', '.join(str(c) for c in sorted(case_values))))

def find_quantile(value, seq):
    seq = sorted(seq)
    if value not in seq:
        seq.append(value)
        seq.sort()
    return seq.index(value) / (len(seq) - 1)

def find_quantile_index(qtile, seq):
    if 0 <= qtile <= 1:
        return seq.index(seq[int(qtile * (len(seq) - 1))])
    else:
        raise ValueError(f'Quantile expected to be between 0 and 1, received {qtile}.')

def validate_index_bounds(ind, bound):
    return max(0, ind-bound), ind+bound

def get_quantile_range(remaining_avg, round_number, model_dict):
    ## get value associated with current quantile
    dist_qtile = find_quantile(remaining_avg, model_dict[round_number].get('remaining_avg'))
    dist_qtile_index = find_quantile_index(dist_qtile, model_dict[round_number].get('remaining_avg'))

    ## get +/- 1 decile from current quantile
    qtile_bound = find_quantile_index(0.1, model_dict[round_number].get('remaining_avg'))
    lower_bound, upper_bound = validate_index_bounds(dist_qtile_index, qtile_bound)

    ## slice sample range from data and return data
    return model_dict[round_number].get('offer_avg_ratio')[lower_bound:upper_bound]

def calculate_banker_offer(round_number, cases,  model_dict=model, n_sims=30):
    ## calculate current average
    remaining_avg = sum(cases.values()) / len(cases)

    ## get slice of distribution based on current values
    qtile_range = get_quantile_range(remaining_avg, round_number, model_dict)

    ## bootstrap results and generate offer
    bootstrapped_offer_ratio = sum(random.choices(qtile_range, k=n_sims)) / n_sims

    return remaining_avg * bootstrapped_offer_ratio