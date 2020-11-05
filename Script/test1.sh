echo $#
echo $*
echo $@
case $1 in
	M|m)
		echo "Man";;
	F|f|w|W)
		echo "Woman";;
	*)
		echo "What?"
esac

shift

read -p "print a number:" yn
case $yn in
	1)
		echo 1 was print;;
	2)
		echo 2 was print;;
	*)
		echo w
esac

for i;do echo $i;done


