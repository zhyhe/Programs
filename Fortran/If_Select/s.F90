        implicit none
        integer::score
        print *,"请输入学生成绩："
        read(*,*) score
        !选择结构语句
        if(score<60) then
                print *,"等级为C"
        else if(score>=60 .and. score<80) then
                print *,"等级为B"
        else 
                print *,"等级为A"
        end if
end
