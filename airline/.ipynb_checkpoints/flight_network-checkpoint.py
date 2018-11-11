import pandas as pd
import networkx as nx

# load data
airlines = pd.read_csv("airlines.csv")
countries = pd.read_csv("countries_of_the_world.csv")
routes = pd.read_csv("routes.csv")

flight_graph = nx.Graph()
flight_graph.add_nodes_from(airlines["ICAO"].unique().tolist())
flight_graph.add_edges_from(list(zip(routes["source_airport"], routes["destination_apirport"])))
