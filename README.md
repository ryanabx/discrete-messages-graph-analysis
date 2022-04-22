# Java Discrete Messages Analysis

This project statically analyzes Java code. Input a project directory and the program will locate all .java files, find **classes, interfaces, enums, and records**, find **variables that are of any detected class type** (only includes classes/interfaces/enums/records found during the first pass), find **public methods** in each class, and then use that information to find the **amount of times any method from a class is called from another class**.

> **_Note:_** This program was developed for specific purposes of research, if there are any issues, they may or may not get resolved without manual checking of the code! Hopefully the code works properly without a hitch though!