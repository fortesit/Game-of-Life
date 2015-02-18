c Main: Game of Life
      program Game of Life

c Declarations
	  character*80 fname,temp,patternName
	  character pattern(0:101,0:81),nextPattern(0:101,0:81)
	  integer nGen,row,col,i,j,ios,pos

c Initialization
	  data pattern/ 8364 * '0'/
	  data nextPattern/ 8364 * '0'/

c Read input file
      call getarg(1,fname)
      open(unit=1, file=fname, status='old', iostat=ios, err=999)
	  read(1,'(A)') patternName
	  pos=1
  360 pos=pos+1
	  if(patternName(pos:pos).NE.'\r') goto 360
	  read(1,*) nGen
	  read(1,*) row,col
	  open(unit=2, file=patternName(1:pos-1)//'for.txt', status='new')
	  i=1
   20 j=1
      read(1,'(A)') temp
   10  pattern(i,j)=temp(j:j)
	   j=j+1
	   if(j.LE.col) goto 10
	  i=i+1
	  if(i.LE.row) goto 20
	  call phaser(pattern,nextPattern,nGen,row,col)
      
	  close(1)
  999 if(ios.EQ.0) goto 1000
       write(*,'(A40)') 'Bad input file. Program exit peacefully.'
 1000 stop
      end

c Subroutine: Pattern phaser
      subroutine phaser(pattern,nextPattern,nGen,row,col)

c Intake parameters
      character pattern(0:101,0:81),nextPattern(0:101,0:81)
	  integer nGen,row,col

c Subroutine local variable
	  integer i,j,k,alifeCellsCount,areSamePatterns

c Restore values of pattern(i,j) and nextPattern(i,j) for later compution and check if it is still life
      k=0
  110 if(k.EQ.0) goto 150
       areSamePatterns=1
       i=0
  130   j=0
  140    if(pattern(i,j).EQ.nextPattern(i,j)) goto 160
          if(k.EQ.nGen+1) goto 340
           pattern(i,j)=nextPattern(i,j)
  340     areSamePatterns=0
  160    nextPattern(i,j)='0'
         j=j+1
	     if(j.LE.81) goto 140
	    i=i+1
	    if(i.LE.101) goto 130
	    if(k.EQ.nGen+1) goto 350
       if(areSamePatterns.EQ.1) goto 180

c Compute next generation status
  150 i=1
   60  j=1
   50   alifeCellsCount=0
        call countAlifeCells(pattern,i,j,alifeCellsCount)
        if(alifeCellsCount.NE.2.AND.alifeCellsCount.NE.3) goto 100
	     if(pattern(i,j).NE.'*'.AND.alifeCellsCount.EQ.2) goto 100
          nextPattern(i,j)='*'
  100   j=j+1
	    if(j.LE.col) goto 50
	   i=i+1
	   if(i.LE.row) goto 60
	  k=k+1
	  if(k.LE.nGen) goto 110

c Output results to file
  180 call outputPattern(pattern,row,col)
	  if(areSamePatterns.EQ.1) goto 250
	   goto 110
  350  if(areSamePatterns.EQ.1) goto 250
	    write(2,'(A40$)') 'It is still not a still life even after '
        if(nGen.GE.10) goto 190
         write(2,'(I1$)') nGen
		 goto 230
  190   if(nGen.GE.100) goto 200
         write(2,'(I2$)') nGen 
		 goto 230
  200   if(nGen.GE.1000) goto 210
         write(2,'(I3$)') nGen 
		 goto 230
  210   if(nGen.GE.10000) goto 220
         write(2,'(I4$)') nGen 
		 goto 230
  220   write(2,'(I5$)') nGen 
  230   write(2,'(A5$)') ' step'
	    if(nGen.EQ.1) goto 240
	     write(2,'(A1$)') 's'
  240   write(2,'(A2)') '.\r'
        goto 330
  250 write(2,'(A19$)') 'It is a still life '
      if(k.NE.1) goto 260
       write(2,'(A11)') 'initially.\r'
	   goto 330
  260 write(2,'(A6$)') 'after '
      if(k.GE.10) goto 270
       write(2,'(I1$)') k-1
       goto 310
  270 if(k.GE.100) goto 280
       write(2,'(I2$)') k-1
       goto 310
  280 if(k.GE.1000) goto 290
       write(2,'(I3$)') k-1
       goto 310
  290 if(k.GE.10000) goto 300
       write(2,'(I4$)') k-1
	   goto 310
  300 write(2,'(I5$)') k-1
  310 write(2,'(A5$)') ' step'
      if(k-1.EQ.1) goto 320
       write(2,'(A1$)') 's'
  320 write(2,'(A2)') '.\r'
  330 return
	  end

c Subroutine: Count the number of alife cells nearby
	  subroutine countAlifeCells(pattern,i,j,alifeCellsCount)

c Intake parameters
	  character pattern(0:101,0:81)
	  integer i,j,alifeCellsCount

c Subroutine local variable
	  integer k,l

c Count
	  k=i-1
   80  l=j-1
   90  if(pattern(k,l).NE.'*') goto 70
	    if(k.EQ.i.AND.l.EQ.j) goto 70
	     alifeCellsCount=alifeCellsCount+1
   70  l=l+1
       if(l.LE.j+1) goto 90
	  k=k+1
	  if(k.LE.i+1) goto 80

	  return
	  end

c Subroutine: Output pattern
	  subroutine outputPattern(pattern,row,col)

c Intake parameters
	  character pattern(0:101,0:81)
	  integer row,col

c Subroutine local variable
	  integer i,j

c Output
	  i=1
  120 write(2,'(82A$:A1)') (pattern(i,j),j=1,col),'\r'
      i=i+1
	  if(i.LE.row) goto 120
	  
	  return
	  end
