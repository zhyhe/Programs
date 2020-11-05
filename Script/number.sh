echo $1
echo $2
echo $3
echo `ls --color=auto /home/zhyhe/workspace`
echo "$PATH"
echo '$PATH'
echo \$PATH
for animals in cat dog
do
	echo "$animals"s
done
for i in $(seq 1 5)
do
	echo $i
done
s=0
for(( i = 1; i <= 10; i++ ))
do
	s=$(($s+$i))
	echo $s
done
echo $s
