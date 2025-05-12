# A simple R script to greet the user

# Ask for the user's name
user_name <- readline(prompt = "What is your name? ")

hometown <- readline(prompt = "What is the name of your hometown and state/country?")

# Create a customized greeting message
greeting <- paste("Hello,", user_name, "from", hometown, "! Welcome to the world of R!")

# Print the greeting message
print(greeting)
