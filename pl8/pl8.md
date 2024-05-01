# Sequential Erlang

## Basic Problems
Create the following functions:

* square/1, given a number returns its square value.
* temp_convert/1, given a temperature value in Fahrenheit convert it to Celsius (knowing that, 5(F-32) = 9C).

## Recursive Problems
Create the following functions:

* factorial/1, given a number return its factorial value.
* fib/1, given a number return its Fibonacci value.

## Lists
Create the following functions:

* count/1, given a list returns the number of elements.
* member/2, returns true if a given element belongs to a given list or false otherwise.
* delete/2, deletes a given element from a given list.
* reverse/1, given a list returns another list with the elements from the first list in the opposite order.
* average/1, given a list of numbers, return the average of its values.

## Guards
Use guards to solve the following problems:

* sum_max/2, given two lists, finds the higher value of each of them and returns their sum.
* int/1, given a list with integers and floats, return a new list containing only integers.

## Tuples
Given a list of tuples that stores data about students (names and the grade they obtained in SOCOF), create a function grades/1 that returns the average of all students, the name and grade of the student with the highest score and the name and grade of the student with the lowest score.

## Perspective
This assignment introduces sequential programming in the functional language Erlang. In summary, upon completion you should understand:

* How variables work.
* How to create functions, modules and export functions from modules.
* How to filter the input of a function by using "guards".
* How to organize collections of objects in lists and tuples.
* Understand the difference between lists and tuples.