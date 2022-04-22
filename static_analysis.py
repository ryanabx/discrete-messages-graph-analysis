# Statically analyzes Java code to find the number of "discrete" messages sent between classes.
# A discrete message means a unique method of another class is called.
# Code does not take into account method overloading
# Kinda just put this together in a day, it will probably have issues.
# Ryan Brue 2022

from os import listdir
from os.path import isfile, join
import os
import json

class_structure : dict = {}

def get_all_files(dir_path):
    all_files : list = list()

    files_in_dir = os.listdir(dir_path)

    for entry in files_in_dir:
        full_path = os.path.join(dir_path, entry)

        if os.path.isdir(full_path):
            all_files = all_files + get_all_files(full_path)
        else:
            all_files.append(full_path)
    return all_files

def static_analysis(dir_path, save_path):
    print(f'Chosen path: {dir_path}')
    files_list = get_all_files(dir_path)

    dir_path = dir_path.replace("\\", "/")
    for f in range(len(files_list)):
        files_list[f] = files_list[f].replace("\\", "/")

    java_files = []
    java_files_abbreviated = []

    for f in files_list:
        if '.java' in f:
            java_files.append(f)
            java_files_abbreviated.append(f'{f}'.split(dir_path, 1)[1])
    
    if len(java_files) < 20:
        print(f'All .java files found: {java_files_abbreviated}')
    else:
        print(f'Found {len(java_files)} .java files!')
    
    num_files = len(java_files)
    
    print("Extracting classes... ")
    i = 0
    for f in java_files:
        extract_classes(open(f'{f}', "r", encoding = "utf8"))
        i += 1
        if i % (num_files // 100) == 0:
            print(f'{i * 100 // num_files}% finished')
        
    print("Done!\nExtracting variables... ")

    i = 0
    for f in java_files:
        extract_variables(open(f'{f}', "r", encoding = "utf8"), f.split('.java', 1)[0].rsplit('/', 1)[1])
        i += 1
        if i % (num_files // 100) == 0:
            print(f'{i * 100 // num_files}% finished')
        
    print("Done!\nExtracting methods... ")

    i = 0
    for f in java_files:
        extract_methods(open(f'{f}', "r", encoding = "utf8"), f.split('.java', 1)[0].rsplit('/', 1)[1])
        i += 1
        if i % (num_files // 100) == 0:
            print(f'{i * 100 // num_files}% finished')
    
    print("Done!\nExtracting discrete messages... ")

    i = 0
    for f in java_files:
        extract_discrete_messages(open(f'{f}', "r", encoding = "utf8"), f.split('.java', 1)[0].rsplit('/', 1)[1])
        i += 1
        if i % (num_files // 100) == 0:
            print(f'{i * 100 // num_files}% finished')
    
    print("Done!")

    # print(class_structure)
    with open(save_path, "w") as outfile:
        json.dump(class_structure, outfile)
    print("Saved file!")

def extract_discrete_messages(file1, name_class):
    lines = file1.readlines()
    for line in lines:
        if '//' in line:
            line = line.split('//', 1)[0]
        if '* ' in line:
            line = line.split('* ', 1)[0]
        if '/*' in line:
            line = line.split('/*', 1)[0]
        if '*/' in line:
            line = line.split('*/', 1)[0]
        
        line = line.strip()

        if name_class not in class_structure:
            return

        if 'variables' not in class_structure[name_class]:
            return
        
        for variable_info in class_structure[name_class]['variables']:
            if f'{variable_info["name"]}' in line:
                for class_method in class_structure[variable_info['type']]['methods']:
                    if f'{variable_info["name"]}' in line and f'.{class_method["name"]}' in line:
                        if variable_info['type'] not in class_structure[name_class]['discrete_messages']:
                            class_structure[name_class]['discrete_messages'][variable_info['type']] = {}
                        if class_method["name"] not in class_structure[name_class]['discrete_messages'][variable_info['type']]:
                            class_structure[name_class]['discrete_messages'][variable_info['type']][class_method["name"]] = 0
                        class_structure[name_class]['discrete_messages'][variable_info['type']][class_method["name"]] += 1

    
def extract_methods(file1, name_class):
    lines = file1.readlines()

    for line in lines:
        if '//' in line:
            line = line.split('//', 1)[0]
        if '* ' in line:
            line = line.split('* ', 1)[0]
        if '/*' in line:
            line = line.split('/*', 1)[0]
        if '*/' in line:
            line = line.split('*/', 1)[0]
        
        line = line.strip()
        
        if '(' in line and ";" not in line:
            curr_line = line
            method = {}
            curr_line = curr_line.split('(', 1)[0]
            if 'public ' in curr_line:
                method['visibility'] = "public"
                curr_line = curr_line.split("public ", 1)[1]
                if curr_line.split(" ")[0] == "static":
                    method['static'] = True
                    curr_line = curr_line.split("static ", 1)[1]
                else:
                    method['static'] = False
                # print(f'Current Line: {curr_line}')
                if " " in curr_line:
                    x = curr_line.split(" ")
                    
                    method['return_type'] = curr_line.rsplit(" ", 1)[0]
                    curr_line = curr_line.rsplit(" ", 1)[1]
                    curr_line = curr_line.replace(" ", "")
                    
                else:
                    method['return_type'] = "constructor"
                
                method['name'] = curr_line
                class_structure[name_class]['methods'].append(method)

                
                
                

def extract_classes(file1):
    lines = file1.readlines()
    scope_level = 0
    class_name : str = None
    commented_out = False
    for line in lines:

        orig_line = line

        if '//' in line:
            line = line.split('//', 1)[0]
        if '* ' in line:
            line = line.split('* ', 1)[0]
        if '/*' in line:
            line = line.split('/*', 1)[0]
        if '*/' in line:
            line = line.split('*/', 1)[0]
        
        line = line.strip()
        
        if commented_out:
            if '*/' in orig_line:
                commented_out = False
            else:
                continue

        if 'class ' in line and 'private ' not in line and '.class' not in line:
            try:
                class_name = line.split('class ', 1)[1].split(' ', 1)[0]
            except Exception as e:
                print(e)
                print(line)
                quit()
            
            if "<" in class_name:
                class_name = class_name.split("<", 1)[0]
            
            class_structure[class_name] = {}
            if 'extends ' in line:
                extends = line.split('extends ', 1)[1]
                if ' ' in extends:
                    extends = extends.split(' ', 1)[0]
                elif '{' in extends:
                    extends = extends.split('{', 1)[0]
                class_structure[class_name]['extends'] = extends
            
            if 'implements ' in line:
                implements = line.split('implements ', 1)[1]
                if ' ' in implements:
                    implements = implements.split(' ', 1)[0]
                elif '{' in extends:
                    implements = implements.split('{', 1)[0]
                class_structure[class_name]['implements'] = implements
            
            class_structure[class_name]['variables'] = []
            class_structure[class_name]['methods'] = []
            class_structure[class_name]['discrete_messages'] = {}
            break

        if 'enum ' in line and 'private ' not in line:
            try:
                class_name = line.split('enum ', 1)[1].split(' ', 1)[0]
            except Exception as e:
                print(e)
                print(line)
                quit()
            
            if "<" in class_name:
                class_name = class_name.split("<", 1)[0]
            
            class_structure[class_name] = {}
            if 'extends ' in line:
                extends = line.split('extends ', 1)[1]
                if ' ' in extends:
                    extends = extends.split(' ', 1)[0]
                elif '{' in extends:
                    extends = extends.split('{', 1)[0]
                class_structure[class_name]['extends'] = extends
            
            class_structure[class_name]['variables'] = []
            class_structure[class_name]['methods'] = []
            class_structure[class_name]['discrete_messages'] = {}
            break

        if 'interface ' in line and 'private ' not in line:
            try:
                class_name = line.split('interface ', 1)[1].split(' ', 1)[0]
            except Exception as e:
                print(e)
                print(line)
                quit()
            
            if "<" in class_name:
                class_name = class_name.split("<", 1)[0]
            
            class_structure[class_name] = {}
            if 'extends ' in line:
                extends = line.split('extends ', 1)[1]
                if ' ' in extends:
                    extends = extends.split(' ', 1)[0]
                elif '{' in extends:
                    extends = extends.split('{', 1)[0]
                class_structure[class_name]['extends'] = extends
            
            class_structure[class_name]['variables'] = []
            class_structure[class_name]['methods'] = []
            class_structure[class_name]['discrete_messages'] = {}
        
        if 'record ' in line and 'private ' not in line:
            try:
                class_name = line.split('record ', 1)[1].split('(', 1)[0]
            except Exception as e:
                print(e)
                print(line)
                quit()
            
            if "<" in class_name:
                class_name = class_name.split("<", 1)[0]
            
            class_structure[class_name] = {}
            if 'extends ' in line:
                extends = line.split('extends ', 1)[1]
                if ' ' in extends:
                    extends = extends.split(' ', 1)[0]
                elif '{' in extends:
                    extends = extends.split('{', 1)[0]
                class_structure[class_name]['extends'] = extends
            
            class_structure[class_name]['variables'] = []
            class_structure[class_name]['methods'] = []
            class_structure[class_name]['discrete_messages'] = {}
        if "/*" in orig_line:
            commented_out = True
        if "*/" in orig_line:
            commented_out = False
            
    

def extract_variables(file1, name_class):
    lines = file1.readlines()
    scope_level = 0

    variable : dict = {
        'visibility': None,
        'name': None,
        'type': None
    }

    linenum = 1
    
    for line in lines:
        orig_line = line

        line = line.strip()

        if '//' in line:
            line = line.split('//', 1)[0]
        if '* ' in line:
            line = line.split('* ', 1)[0]
        if '/*' in line:
            line = line.split('/*', 1)[0]
        if '*/' in line:
            line = line.split('*/', 1)[0]
        
        if " =" in line:
            line = line.split(" =", 1)[0]

        if "=" in line:
            line = line.split("=", 1)[0]
        
        if "\n" in line:
            line = line.split("\n", 1)[0]
        
        check = False

        for item2 in line.split(" "):
            if "[" in item2:
                item2 = item2.split("[", 1)[0]
            if "<" in item2:
                item2 = item2.split("<", 1)[0]
            
            if item2 in class_structure:
                check = True
        
        check2 = False

        if '(' not in line:
            check2 = True
        
        # print(f'{orig_line} - {check} and {check2} and {";" in orig_line}')
        
        if (check and check2) and (";" in orig_line):
            if ";" in line:
                line = line.split(";", 1)[0]
            
            variables = []
            variable = {
                'visibility': None,
                'name': None,
                'type': None,
                'static': None
            }
            if 'public' in line:
                variable['visibility'] = "public"
            elif 'private' in line:
                variable['visibility'] = "private"
            elif 'protected' in line:
                variable['visibility'] = "protected"
            else:
                variable['visibility'] = ""
            
            line_split = line

            if variable['visibility'] != "":
                line_split = line.split(variable['visibility'], 1)[1]

            if 'static ' in line_split:
                variable['static'] = True
                line_split = line_split.split("static ")[1]
            else:
                variable['static'] = False
            
            # print(line_split)

            line_split = line_split.replace("\t", "")

            if ' ' in line_split and line_split.index(' ') == 0:
                line_split = line_split.split(' ', 1)[1]

            for item in class_structure.keys():
                array_check = f'{item}[' in line_split # and line_split.index(f'{item}[') == 0
                space_check = f'{item} ' in line_split # and line_split.index(f'{item} ') == 0
                
                if (array_check or space_check):
                    variable['type'] = item
                    # print(f'Item: {item}')
                    # print(f'{line_split}')
                    line_split = line_split.rsplit(' ', 1)[1]
                    break
            
            if not variable['type']:
                continue
            
            
            # print(line_split)

            if ' ' in line_split and line_split.index(' ') == len(line_split) - 1:
                    line_split = line_split.split(' ', 1)[0]
            
            var_names = []

            line_split = line_split.replace(" ", "")
            line_split = line_split.replace(";", "")

            # print(f'Line split: {line_split}')

            if ',' in line_split:
                var_names = line_split.split(',')
            else:
                var_names = [line_split]

            # print(var_names)

            for var_name in var_names:
                variable['name'] = var_name
                if '{' in variable['name']:
                    variable['name'] = variable['name'].split('{', 1)[0]
                
                if ';' in variable['name']:
                    variable['name'] = variable['name'].split(';', 1)[0]
                
                variable['scope_level'] = scope_level
                variable['line'] = linenum
                class_structure[name_class]['variables'].append({
                    'visibility': variable['visibility'],
                    'type': variable['type'],
                    'name': variable['name'],
                    'line': variable['line'],
                    'scope_level': variable['scope_level']
                })

        if '{' in orig_line:
            scope_level += 1
        if '}' in orig_line:
            scope_level -= 1
        
        linenum += 1
            


def main():
    print(os.getcwd())
    print("Static Code Analysis for Number of Discrete Messages Between Classes")
    print("Enter the directory to check Java code from. (The code will do a search of ALL SUBDIRECTORIES): ", end = '')
    curr_directory = input()
    print("\nEnter the path for the resulting file to go in: ", end = '')
    save_dir = input()
    static_analysis(curr_directory, f'{save_dir}/result.json')
    print("Done!")

if __name__ == "__main__":
    main()