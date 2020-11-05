pro main
lww=20L
openw,lww,'wy.txt'
j=eof(lww)
print,j
printf,lww,1
printf,lww,2
printf,lww,3
printf,lww,"I love her"
j=eof(lww)
print,j
close,lww

openu,lww,'wy.txt',/get_lun
j=eof(lww)
print,j
k=0
while(not eof(lww)) do begin
    readf,lww,kkk
    k=k+1
    print,'kkk',k
endwhile
print,eof(lww),'k=',k,format='(I8,/,A,I2)'
close,lww
free_lun,lww
end
