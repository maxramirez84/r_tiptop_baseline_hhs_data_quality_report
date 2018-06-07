
# INPUTS: API TOKEN
# OUTPUT: CSV FILE WITH DATA SET WITHOUT DUPLICATES

# Read arguments

# Read data set from REDCap by using the provided token

# Remove duplicated records (where all variables contain exactly the same values except record_id)

# Analyze reused household IDs, where they are duplicates or not

# When they are not duplicates, reassing a new household ID

# When they are duplicates, remove them

# Report what have been done