pro Table
for i=1,9 do begin
    for j=1,i do begin
        print,j,'x',i,'=',i*j,format='(4X,I1,1X,A1,1X,I1,1X,A1,1X,I2,$)'
    endfor
    print,format='(/)'
endfor
end
