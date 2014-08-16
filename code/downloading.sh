#!/bin/bash

### when supplied with four arguments in the form xxLAT xxxLONG yyLAT 
### yyyLONG where xs give the top left 10deg grid square, and ys give 
### lower 10 deg grid square, will produce a text file of htmls to down 
### -load Hansen data from, and then use wget in access

# accepting arguments from terminal, and assigning to variables
T_lat=$1
T_long=$2
B_lat=$3
B_long=$4

# separating into direction (N-W, E-S) and magnitudes for: top left
T_lat_dir=`echo -n $T_lat | tail -c 1`
T_lat_mag=$(echo -n $T_lat | head -c 2)
T_long_dir=`echo -n  $T_long | tail -c 1`
T_long_mag=`echo -n $T_long | head -c 3`

# separating into direction (N-W, E-S) and magnitudes for: bottom right
B_lat_dir=`echo -n $B_lat | tail -c 1`
B_lat_mag=`echo -n $B_lat | head -c 2`
B_long_dir=`echo -n  $B_long | tail -c 1`
B_long_mag=`echo -n $B_long | head -c 3`

# Arrays to be appended to for lats and longs, respectively:
LAT=()
LAT_dir=()
LONG=()
LONG_dir=()

# Latitudes:
# three possibilities: (1) start is N and finish is S, (2) start is 
# N and finish is N, and (3) start is S and finish is S. Deal with
# each of these in turn. Also (4) start is on equator (written N)

# first, need to format inputs:
# If on the equator, i.e. if 00, make 0
if [ $T_lat_mag = "00" ]; then 
	T_lat_mag="0"
fi
if [ $B_lat_mag = "00" ]; then 
	B_lat_mag="0"
fi

# (1) if the upper is N and the bottom is S
if [ $T_lat_dir = N ] && [ $B_lat_dir = S ]; then
	#while loop: while this != last, carry on going down
	#setting the first latitude coords
	mag_current=$T_lat_mag
    while [ $mag_current != "0" ]; do
		#
		LAT+=($mag_current)
		LAT_dir+=(N)
		#output will go here
		mag_current=`expr $mag_current - 10`
        done
    #
    LAT+=($mag_current)
    LAT_dir+=(N)
    #add the final coords, the while loop having stopped
    #here stops at equator, if min is S, move to another loop which will 
    #count up going southerly
    if [ $B_lat_dir = S ]; then
		mag_current=10
		while [ $mag_current != $B_lat_mag ]; do
			#
			LAT+=($mag_current)
			LAT_dir+=(S)
			#send html to text file 
			mag_current=`expr $mag_current + 10`
			done
		#
		LAT+=($mag_current)
		LAT_dir+=(S)
		# add the final coords, the while loop having stopped
	fi
fi

# (2) if the upper is N and the bottom is N
if [ $T_lat_dir = N ] && [ $B_lat_dir = N ]; then
	#while loop: while this != last, carry on going down
	#setting the first latitude coords
	mag_current=$T_lat_mag
    while [ $mag_current != $B_lat_mag ]; do
		#
		LAT+=($mag_current)
		LAT_dir+=(N)
		#output will go here
		mag_current=`expr $mag_current - 10`
        done
    #
    LAT+=($mag_current)
    LAT_dir+=(N)
    #add the final coords, the while loop having stopped
fi

# (3) if the upper is S and the bottom is S
if [ $T_lat_dir = S ] && [ $B_lat_dir = S ]; then
	#while loop: while this != last, carry on going down
	#setting the first latitude coords
	mag_current=$T_lat_mag
    while [ $mag_current != $B_lat_mag ]; do
		#
		LAT+=($mag_current)
		LAT_dir+=(S)
		#output will go here
		mag_current=`expr $mag_current + 10`
        done
    #
    LAT+=($mag_current)
    LAT_dir+=(S)
    #add the final coords, the while loop having stopped
fi

# Longitudes:
# three possibilities: (1) start is W and finish is E, (2) start is 
# W and finish is W, and (3) start is E and finish is E. Deal with
# each of these in turn.

# first, need to format inputs:
# if first digit of three digit input is zero, remove. If on the equator
# , i.e. 000, make this 0.
T_long_mag1=`echo $T_long_mag | head -c 1`
T_long_mag23=`echo -n $T_long_mag | tail -c 2`
if [ $T_long_mag = "000" ]; then 
	T_long_mag=0
else if [ $T_long_mag1 = 0 ]; then 
	T_long_mag=$T_long_mag23
	fi
fi

B_long_mag1=`echo $B_long_mag | head -c 1`
B_long_mag23=`echo -n $B_long_mag | tail -c 2`
if [ $B_long_mag = "000" ]; then 
	B_long_mag="0"
else if [ $B_long_mag1 = "0" ]; then 
	B_long_mag=$B_long_mag23
	fi
fi

# (1) if the upper is W and the finish is E
if [ $T_long_dir = W ] && [ $B_long_dir = E ]; then
	#while loop: while this != last, carry on going down
	#setting the first latitude coords
	mag_current=$T_long_mag
    while [ $mag_current != "10" ]; do
		#
		LONG+=($mag_current)
		LONG_dir+=(W)
		#output will go here
		mag_current=`expr $mag_current - 10`
        done
    #
    LONG+=($mag_current)
    LONG_dir+=(W)
    #add the final coords, the while loop having stopped
    #here stops at equator, if min is S, move to another loop which will 
    #count up going southerly
	mag_current=0
	while [ $mag_current != $B_long_mag ]; do
		#
		LONG+=($mag_current)
		LONG_dir+=(E)
		#send html to text file 
		mag_current=`expr $mag_current + 10`
		done
	#
	LONG+=($mag_current)
	LONG_dir+=(E)
	# add the final coords, the while loop having stopped
fi

# (2) if the upper is W and the finish is W
if [ $T_long_dir = W ] && [ $B_long_dir = W ]; then
	#setting the first latitude coords
	mag_current=$T_long_mag
    while [ $mag_current != $B_long_mag ]; do
		#
		LONG+=($mag_current)
		LONG_dir+=(W)
		#output will go here
		mag_current=`expr $mag_current - 10`
        done
    LONG+=($mag_current)
    LONG_dir+=(W)
    #add the final coords, the while loop having stopped
fi

# (3) if the upper is E and the finish is E
if [ $T_long_dir = E ] && [ $B_long_dir = E ]; then
	#setting the first latitude coords
	mag_current=$T_long_mag
    while [ $mag_current != $B_long_mag ]; do
		LONG+=($mag_current)
		LONG_dir+=(E)
		#output will go here
		mag_current=`expr $mag_current + 10`
        done
    #
    LONG+=($mag_current)
    LONG_dir+=(E)
    #add the final coords, the while loop having stopped
fi

#
LAT_len=`expr ${#LAT[@]} - 1`
LONG_len=`expr ${#LONG[@]} - 1`
# rewriting LONGs as three-digit
for i in `seq 0 $LONG_len`;
	do
		if [ ${#LONG[i]} != 3 ]; then 
			LONG[i]="0"${LONG[i]}
		fi
		#having been picked up by the first if, now search for 00
		if [ "${LONG[i]}" = "00" ]; then 
			LONG[i]="000"
		fi
	done

# rewriting LATs as two-digit
for i in `seq 0 $LAT_len`;
	do
		if [ "${LAT[i]}" = "0" ]; then
			LAT[i]="00"
		fi
	done

# writing to text file
#first, removing old files
rm ./downloadedTiles/lossYear.txt
rm ./downloadedTiles/treeCover.txt

cover="http://commondatastorage.googleapis.com/earthenginepartners-hansen/GFC2013/Hansen_GFC2013_treecover2000_"
loss="http://commondatastorage.googleapis.com/earthenginepartners-hansen/GFC2013/Hansen_GFC2013_lossyear_"
mask="http://commondatastorage.googleapis.com/earthenginepartners-hansen/GFC2013/Hansen_GFC2013_datamask_"
LONG_out=()

for i in `seq 0 $LAT_len`;
	do 
		for j in `seq 0 $LONG_len`;
			do
				echo "$cover${LAT[i]}${LAT_dir[i]}_${LONG[j]}${LONG_dir[j]}.tif" \
					>> "../downloadedTiles/treeCover.txt"
				echo "$loss${LAT[i]}${LAT_dir[i]}_${LONG[j]}${LONG_dir[j]}.tif" \
					>> "../downloadedTiles/lossYear.txt"
				echo "$mask${LAT[i]}${LAT_dir[i]}_${LONG[j]}${LONG_dir[j]}.tif" \
					>> "../downloadedTiles/dataMask.txt"
				#concatenating direction and magnitude for terminal output
				if [ $i = 1 ]; then 
					LONG_out+=(${LONG[j]}${LONG_dir[j]})
				fi
			done
		#concatenating direction and magnitude for terminal output
		LAT_out+=(${LAT[i]}${LAT_dir[i]})
	done

#terminal output
echo "Longitude range:" ${LONG_out[*]}
echo "Latitude range:" ${LAT_out[*]}

#removing files from downloadedTiles directory 
#rm ../downloadedTiles/lossYear/*.tif
#rm ../downloadedTiles/treeCover/*.tif
#rm ../downloadedTiles/dataMask/*.tif

#downloading files to downloadedTiles directory 
wget -i ../downloadedTiles/lossYear.txt -P ../downloadedTiles/lossYear/ 
wget -i ../downloadedTiles/dataMask.txt -P ../downloadedTiles/dataMask/ 
wget -i ../downloadedTiles/treeCover.txt -P ../downloadedTiles/treeCover/

echo DONE
