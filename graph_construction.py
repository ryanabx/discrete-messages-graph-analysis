import json
import os

def construct_graph_text(RESULT_PATH = ""):
    RESULT = json.load(open(RESULT_PATH, "r"))

    CONVERTED_RESULT = {}

    for class_name in RESULT:
        CONVERTED_RESULT[class_name] = {}
        if 'discrete_messages' not in RESULT[class_name]:
            continue
        for class_name_2 in RESULT[class_name]['discrete_messages']:
            CONVERTED_RESULT[class_name][class_name_2] = len(RESULT[class_name]['discrete_messages'][class_name_2])
    
    return CONVERTED_RESULT

def save_graph_as_dict(RESULT_PATH = "", SAVE_PATH = ""):
    context = construct_graph_text(RESULT_PATH)
  
    with open(SAVE_PATH, "w") as outfile:
        json.dump(context, outfile)

# Graph is saved as Node1 Weight Node2
def save_graph_as_txt(RESULT_PATH = "", TXT_PATH = "", TXT2_PATH = "", DICT_PATH = ""):
    context = construct_graph_text(RESULT_PATH)

    f = open(TXT_PATH, "w")
    f2 = open(TXT2_PATH, "w")

    name_to_num = {}
    i = 1
    for class_name in context:
        f2.write(f'{class_name}\n')
        name_to_num[class_name] = i
        i += 1
    
    for class_name in context:
        for class_name_2 in context[class_name]:
            f.write(f'{name_to_num[class_name]} {context[class_name][class_name_2]} {name_to_num[class_name_2]}\n')
    
    f.close()
    f2.close()

    with open(DICT_PATH, "w") as outfile:
        json.dump(name_to_num, outfile)
    print("Done!")

def main():
    print(os.getcwd())
    print("Convert Static Code Analysis to Graph")
    print("Enter static code analysis result file: ", end = '')
    result_dir = input()
    print("\nEnter the result txt file path: ", end = '')
    txt_dir = input()
    print("\nEnter the result names txt file path: ", end = '')
    txt2_dir = input()
    print("\nEnter the result json file path: ", end = '')
    json_dir = input()
    save_graph_as_txt(result_dir, txt_dir, txt2_dir, json_dir)
    print("Done!")

if __name__ == "__main__":
    main()