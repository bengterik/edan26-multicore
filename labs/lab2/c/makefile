main:
# gcc -o preflow preflow.c -g -O3 -pthread
# cat ../../data/railwayplanning/secret/0mini.ans
	gcc -o preflow preflow.c -g -O3 -pthread
	time sh check-solution.sh ./preflow
	#./preflow < ../../data/railwayplanning/secret/4huge.in
	@echo PASS all tests

#	time sh check-solution.sh ./preflow		// for all
#	@echo PASS all tests

# ../../data/big:
# 000.ans  000.in  001.ans  001.in  002.ans  002.in

# ../../data/huge:
# 001.ans  001.in

# ../../data/railwayplanning:
# sample  secret

# ../../data/tiny:
# 0.ans  0.in