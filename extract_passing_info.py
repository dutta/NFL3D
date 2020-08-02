import pandas as pd
import re 
import json

p = pd.read_csv("/Users/Rishav/Documents/nflScrapR/parabolizR/data/ngs_passing_play_index.csv")

def get_passer(row):
	i = row['play.playStats']
	val = eval(i)
	for o in val:
		if(o['statId'] == 111 or o['statId'] == 112):
			return o['playerName']

def is_inter(row):
	i = row['play.playStats']
	val = eval(i)
	for o in val:
		if(o['statId'] == 112):
			return True
	return False

def get_recv(row):
	i = row['play.playStats']
	val = eval(i)
	for o in val:
		if(o['statId'] == 115):
			return o['playerName']

def get_recv(row):
	i = row['play.playStats']
	val = eval(i)
	for o in val:
		if(o['statId'] == 115):
			return o['playerName']

def get_yac(row):
	i = row['play.playStats']
	val = eval(i)
	for o in val:
		if(o['statId'] == 113):
			return o['yards']
def total_yards(row):
	i = row['play.playStats']
	val = eval(i)
	for o in val:
		if(o['statId'] == 15 or o['statId'] == 16):
			return o['yards']
def air_yards(row):
	return row['yards'] - row['yac']


p['passer'] = p.apply(get_passer, axis = 1)
p['isInterception'] = p.apply(is_inter, axis = 1)
p['recv'] = p.apply(get_recv, axis = 1)
p['yac'] = p.apply(get_yac, axis = 1)
p['yards'] = p.apply(total_yards, axis = 1)
p['air_yards'] = p.apply(air_yards, axis = 1)

p.to_csv("/Users/Rishav/Documents/nflScrapR/parabolizR/data/ngs_passing_play_index_expanded.csv")



