def parse_input(input_str):
    # Clean and split the input string by commas
    items = input_str.strip().split(", ")
    # Remove any extra spaces and return the list
    return [item.strip() for item in items]

def convert_input_to_list(input_string):
    # Remove the outer brackets and spaces
    cleaned_input = input_string.strip('[] ')
    # Split the string into individual elements based on '], ['
    elements = cleaned_input.split('], [')
    # Convert each element into a tuple
    tuple_list = [tuple(element.split(', ')) for element in elements]
    return tuple_list
    
def parse_phi_nodes(input_str):
    phi_nodes_input = convert_input_to_list(input_str)
    cleaned_list = [(item[0].strip(), item[1].strip()) for item in phi_nodes_input]
    return cleaned_list

def main():
    # Take user input for preds
    preds_input = input("Enter preds, separated by commas: ")
    preds = parse_input(preds_input)
    # Take user input for phi nodes
    print("Enter phi nodes in the format '[value, pred]' without quotes and separated by commas.")
    phi_nodes_input = input("Enter phi nodes: ")
    # Process the input to extract phi nodes
    phi_nodes = parse_phi_nodes(phi_nodes_input)
    # print(phi_nodes)
    # Check each phi node's second value to see if it's covered in preds
    uncovered_phi_nodes = [phi for phi in phi_nodes if phi[1] not in preds]

    # Print uncovered phi nodes
    if uncovered_phi_nodes:
        for node in uncovered_phi_nodes:
            print(f"Uncovered phi node: {node}")
    else:
        print("All phi nodes are covered by the given preds.")

if __name__ == "__main__":
    main()
