# Useful scripts

This repository contains scripts that are useful to work with MATSim inputs and outputs.

## analysis-tools

For analysing outputs from the various repositories

## event tools

For processing MATSim event outputs

## network tools

For processing the network

## plan tools

For processing plans

## sa3-to-sa1

This code nominates an sa1 region for each sa3 regions. The preference is sa1 regions with a:

1. Vline station
2. Regional bus stop
3. Metro train station
4. Regular bus stop

Any ties (e.g., two Vline stations in one sa3) are broken by picking the sa1 that’s closest to the center of the sa3 region.
