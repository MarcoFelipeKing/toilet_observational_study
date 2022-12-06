# Code to read in data clean_contacts, split into multiple dataframes based on experimentID
using DataFrames
using CSV
using Plots

# Read in data
clean_contacts = CSV.read("contacts/data/clean_contacts.csv",DataFrame)
