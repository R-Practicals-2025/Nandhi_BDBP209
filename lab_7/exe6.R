# 1 If-else-if structure to determine the quadrant of an angle
angle_degree <- 45

if (angle_degree >= 0 && angle_degree < 90) {
  print('First quadrant')
} else if (angle_degree >= 90 && angle_degree < 180) {
  print('Second quadrant')
} else if (angle_degree >= 180 && angle_degree < 270) {
  print('Third quadrant')
} else if (angle_degree >= 270 && angle_degree < 360) {
  print('Fourth quadrant')
} else {
  print('Invalid angle')
}

# 2 If-else-if structure to arrange three numbers in decreasing order
first_num <- 12
second_num <- 7
third_num <- 15

# Sorting numbers in decreasing order using if-else-if
if (first_num >= second_num & first_num >= third_num) {
  if (second_num >= third_num) {
    print(paste("The numbers in decreasing order: ", first_num, second_num, third_num))
  } else {
    print(paste("The numbers in decreasing order: ", first_num, third_num, second_num))
  }
} else if (second_num >= first_num & second_num >= third_num) {
  if (first_num >= third_num) {
    print(paste("The numbers in decreasing order: ", second_num, first_num, third_num))
  } else {
    print(paste("The numbers in decreasing order: ", second_num, third_num, first_num))
  }
} else {
  if (first_num >= second_num) {
    print(paste("The numbers in decreasing order: ", third_num, first_num, second_num))
  } else {
    print(paste("The numbers in decreasing order: ", third_num, second_num, first_num))
  }
}

# 3 Journey ticket cost calculation based on distance and age
journey_distance <- as.numeric(readline(prompt = "Enter the journey distance (in km): "))
traveller_age <- as.numeric(readline(prompt = "Enter the traveller's age: "))

# Initialize the ticket cost
journey_cost <- 0

# Calculate the base cost based on the distance
if (journey_distance <= 100) {
  journey_cost <- 100  # Fixed cost for the first 100 km
} else if (journey_distance <= 1000) {
  journey_cost <- 100 + (journey_distance - 100) * 1.50  # Rs. 1.50 per km after 100 km
} else {
  journey_cost <- 100 + (1000 - 100) * 1.50 + (journey_distance - 1000) * 2  # Rs. 2 per km after 1000 km
}

# Apply concessions based on age
if (traveller_age > 60) {
  journey_cost <- journey_cost * 0.75  # 25% concession for senior citizens
} else if (traveller_age < 6) {
  journey_cost <- journey_cost * 0.50  # 50% concession for children under 6 years
}

# Print the final ticket cost
print(paste("The total ticket cost is: Rs.", round(journey_cost, 2)))
