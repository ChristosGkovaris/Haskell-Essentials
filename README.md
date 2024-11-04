# Haskell Utility Functions

Welcome to the "Haskell Utility Functions" repository, a collection of useful functions implemented in Haskell. These functions cover various tasks such as mathematical computations, list manipulations, and data processing.

## Functions Overview

### Factorial Calculation
- **Function:** `factorial :: Integer -> Integer`
- **Objective:** Calculate the factorial of a given integer.  
- **Example:** `factorial 5` returns `120`.

### Range Checking
- **Function:** `number_in_range :: Int -> Int -> Int -> Bool`  
- **Objective:** Check if a number is within a specified range.  
- **Example:** `number_in_range 1 5 10` returns `True`.

### Remove Duplicates
- **Function:** `no_duplicates :: [Int] -> [Int]`  
- **Objective:** Remove duplicate integers from a list.  
- **Example:** `no_duplicates [1, 2, 2, 3]` returns `[1, 2, 3]`.

### Generate List Boundaries
- **Function:** `generate_list_boundaries :: Int -> Int -> [Int]`  
- **Objective:** Generate a list of integers within a specified range.  
- **Example:** `generate_list_boundaries 1 5` returns `[1, 2, 3, 4, 5]`.

### Sum List Elements
- **Function:** `sum_all_list_elems :: [Int] -> Int`  
- **Objective:** Calculate the sum of all integers in a list.  
- **Example:** `sum_all_list_elems [1, 2, 3]` returns `6`.

### Delete Even Numbers
- **Function:** `delete_even_numbers_from_list :: [Int] -> [Int]`  
- **Objective:** Remove all even integers from a list.  
- **Example:** `delete_even_numbers_from_list [1, 2, 3, 4]` returns `[1, 3]`.

### Delete Odd Numbers
- **Function:** `delete_odd_numbers_from_list :: [Int] -> [Int]`  
- **Objective:** Remove all odd integers from a list.  
- **Example:** `delete_odd_numbers_from_list [1, 2, 3, 4]` returns `[2, 4]`.

### Element Existence Check
- **Function:** `element_existance_inside_list :: [Int] -> Int -> Bool`  
- **Objective:** Check if a specified integer exists in a list.  
- **Example:** `element_existance_inside_list [1, 2, 3] 2` returns `True`.

### List Sorted Check (Ascending)
- **Function:** `is_list_sorted_ascending :: [Int] -> Bool`  
- **Objective:** Check if a list of integers is sorted in ascending order.  
- **Example:** `is_list_sorted_ascending [1, 2, 3]` returns `True`.

### List Sorted Check (Descending)
- **Function:** `is_list_sorted_descending :: [Int] -> Bool`
- **Objective:** Check if a list of integers is sorted in descending order.
- **Example:** `is_list_sorted_descending [3, 2, 1]` returns `True`.

### Generating Function
- **Function:** `generating :: (Int -> Double) -> Int -> (Double -> Double)`  
- **Objective:** Create a higher-order function based on an input function.  
- **Example:** Use as part of a mathematical computation.

### Map Function with Index
- **Function:** `mapi :: [u]->(u->Int->v)->[v]`  
- **Objective:** Apply a function to each element of a list along with its index.  
- **Example:** `mapi [1, 2, 3] (\x i -> x + i)` returns `[1, 3, 5]`.

### Higher-Order Integral
- **Function:** `integral_higher_order :: (Double->Double) -> Double -> Double -> Double -> Double`  
- **Objective:** Calculate the integral of a function using the trapezoidal rule.  
- **Example:** Use for numerical integration tasks.

### Element Check by Position
- **Function:** `check_element :: [Int] -> Int -> Bool`  
- **Objective:** Check for an element based on its position in the list.  
- **Example:** `check_element [1, 2, 3] 1` returns `True`.

### Element Check by Value
- **Function:** `check_element_given_number :: [Int] -> Int -> Int -> Bool`
- **Objective:** Check if there exists an element less than a given number in the list.  
- **Example:** `check_element_given_number [1, 2, 3] 1 2` returns `True`.

## Implementation Details
- The functions are implemented in Haskell and are designed to handle various tasks related to numerical computations and list manipulations.

## How to Run
- **Clone the Repository:**
  ```bash
     git clone https://github.com/yourusername/Haskell-Essentials.git
     cd Haskell-Essentials
