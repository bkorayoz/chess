	!!! BULENT KORAY OZ - SARPER NALCACI
	!!! 2015 SPRING SEMESTER BIL 106E PROJECT
	!!! CHESS
	!!! This program lets users play chess bilaterally.

	program chess

	parameter (m=9)

51	format (9(3x,i2))

52	format (3x,i1,A51)

53	format (3x,a56)

54  format (3x,a51)

	integer tas(m,m), x1, x2, y1, y2, dif_y, dif_x, a, b, c, ilkoyuncu, oyuncu, sahdurum, kazanan, rokyon

	open (11, file=" chess board2.txt ")   !!! tahtanin dosyadan ekrana yazdirilmasi
							 

	

	do i=1,m

		read(11,*) ( tas(i,j), j=1,m )

	enddo

    print*, "                      HOSGELDINIZ"

	print*
    print*

	print*, "  Bu fortran programi SAH CEKME ve ROK HAMLESI dahil" 
	  
	print*, "iki kisiye karsilikli olarak satranc oynatabilmektedir."

	print*
	
    print*, "SAH CEKILDIGINDE 3 KEZ HATALI HAMLE GIRERSENIZ, KAYBEDERSINIZ."

	print*

    print*,	"  Rok hamlesi icin asagidaki talimatlari uygulayiniz:"

	print*

	print*, "SOL YONLU ROK HAMLESI ICIN <60,60> --> <70,70> GIRINIZ."

	print*, "SAG YONLU ROK HAMLESI ICIN <60,60> --> <80,80> GIRINIZ."

	print*

	print*, " 1. oyuncunun taslari    2. oyuncunun taslari "

	print*

	print*, " 10= piyon			1= piyon "

	print*, " 20= kale			2= kale "

	print*, " 30= at				3= at "

	print*, " 40= fil			4= fil "

	print*, " 50= vezir			5= vezir "

	print*, " 60= sah			6= sah "


	print*

	write(*,53) " Hangi oyuncunun oyuna baslayacagini giriniz. (1 veya 2) "

	read*, ilkoyuncu
		
	do i=1,m

		print*
	
		write(*,51) ( tas(i,j), j=1,m )

		print*
		
	enddo
	
	!!! Oyuncularin hamle girmesi

	do k=ilkoyuncu,1000000

		hamlesayisi=mod(k,2)
	
		if(hamlesayisi.eq.1) then
	
			kisi=1
		
		else
	
			kisi=2
		
		endif	
	
23	write(*,52) kisi, ". oyuncu, oynayayacaginiz tasi girin. (satir,sutun)"
	
	read*, x1, y1
	
	write(*,54) "tasi oynamak istediginiz yeri girin. (satir,sutun)"
	
	read*, x2, y2

	dif_x= x2-x1

	dif_y= y2-y1

	ichange=tas(x2+1,y2+1)

	if(hamlesayisi.eq.1) goto 21

	if(hamlesayisi.eq.0) goto 22
	
	
	!!!
	!!! 1. oyuncunu hamleleri
	!!!
	!!! piyonun hareketi

21	if(tas(x1+1,y1+1).eq.10.and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.1.and.dif_y.eq.0) then !!!duseyde hareket tek kare

		b=10

		call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)

	elseif(tas(x1+1,y1+1).eq.10.and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.2.and.dif_y.eq.0) then	!!! duseyde 2 kare

		if(x1.eq.2) then

			b=10

			call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)

		else

			print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

			goto 23

		endif

	elseif(tas(x1+1,y1+1).eq.10.and.tas(x2+1,y2+1).le.5.and.tas(x2+1,y2+1).ne.0.and.dif_x.eq.1.and.abs(dif_y).eq.1) then !!!tas yeme

			b=10

			call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)

	!!! vezir ve kalenin hareketleri

	elseif((tas(x1+1,y1+1).eq.20.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).eq.0.and.dif_y.eq.0.and.x2+1.gt.x1+1) then !!!duseyde hareket asagi

		do i=1,dif_x
		
			if(tas(x1+1+i,y1+1).eq.0) then

			else
			
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23

			endif
			
		enddo
	
		b=20

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.20.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).eq.0.and.dif_y.eq.0.and.x2+1.lt.x1+1) then !!!duseyde hareket yukari

		do i=1,abs(dif_x)
		
			if(tas(x1+1-i,y1+1).eq.0) then
			
			else
			
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23

			endif
			
		enddo

		b=20

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.20.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.0.and.y1+1.lt.y2+1) then !!!yatay hareket saga

		do i=1,dif_y

			if(tas(x1+1,y1+1+i).eq.0) then
			
			else 
				
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "
				
				goto 23
				
			endif
			
		enddo			

		b=20

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.20.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.0) then !!! yatay hareket sola

		do i=1,abs(dif_y)

			if(tas(x1+1,y1+1-i).eq.0) then
			
			else 
				
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "
				
				goto 23
				
			endif
			
		enddo			
		
		b=20

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.20.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).le.5.and.dif_y.eq.0.and.x2+1.gt.x1+1) then !!! duseyde tas yeme asagi

		do i=1,dif_x-1
		
			if(tas(x1+1+i,y1+1).eq.0) then
			
			else
			
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23

			endif
			
		enddo
		
		b=20

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.20.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).le.5.and.dif_y.eq.0.and.x2+1.lt.x1+1) then !!! duseyde tas yeme yukari

		do i=1,abs(dif_x)-1
		
			if(tas(x1+1-i,y1+1).eq.0) then
			
			else
			
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23

			endif
			
		enddo
		
		b=20

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)


	elseif((tas(x1+1,y1+1).eq.20.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).le.5.and.dif_x.eq.0.and.y1+1.lt.y2+1) then !!!yatay tas yeme saga

		do i=1,dif_y-1

			if(tas(x1+1,y1+1+i).eq.0) then
			
			else 
				
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "
				
				goto 23
				
			endif
			
		enddo			

		b=20

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.20.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).le.5.and.dif_x.eq.0) then !!! yatay tas yeme sola

		do i=1,abs(dif_y)-1

			if(tas(x1+1,y1+1-i).eq.0) then
			
			else 
				
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "
				
				goto 23
				
			endif
			
		enddo
		
		b=20

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	!!!
	!!! vezir ve filin hareketleri
	!!!

	elseif((tas(x1+1,y1+1).eq.40.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.dif_y.and.x1+1.lt.x2+1) then !!!y=-x boyunca hareket asagi

		do i=1,dif_x

			if(tas(x1+1+i,y1+1+i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=40

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.40.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.dif_y) then !!!y=-x boyunca hareket yukari

		do i=1,abs(dif_x)

			if(tas(x1+1-i,y1+1-i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=40

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.40.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).eq.0.and.dif_x.lt.0.and.dif_y.gt.0) then !!!y=x boyunca hareket yukari

		do i=1,dif_y

			if(tas(x1+1-i,y1+1+i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=40

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.40.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).eq.0.and.dif_x.gt.0.and.dif_y.lt.0) then !!!y=x boyunca hareket asagi

		do i=1,dif_x

			if(tas(x1+1+i,y1+1-i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=40

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.40.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).le.5.and.dif_x.eq.dif_y.and.x1+1.lt.x2+1) then !!!y=-x boyunca tas yeme asagi

		do i=1,dif_x-1

			if(tas(x1+1+i,y1+1+i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=40

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.40.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).le.5.and.dif_x.eq.dif_y) then !!!y=-x boyunca tas yeme yukari

		do i=1,abs(dif_x)-1

			if(tas(x1+1-i,y1+1-i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=40

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.40.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).le.5.and.dif_x.lt.0.and.dif_y.gt.0) then !!!y=x boyunca tas yeme yukari

		do i=1,dif_y-1

			if(tas(x1+1-i,y1+1+i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=40

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.40.or.tas(x1+1,y1+1).eq.50).and.tas(x2+1,y2+1).le.5.and.dif_x.gt.0.and.dif_y.lt.0) then !!!y=x boyunca tas yeme asagi

		do i=1,dif_x-1

			if(tas(x1+1+i,y1+1-i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=40

		c=50
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	!!! atin hareketleri (hareket ve tas yeme birlikte)

	elseif(tas(x1+1,y1+1).eq.30.and.(tas(x2+1,y2+1).eq.0.or.tas(x2+1,y2+1).le.5).and.abs(dif_x).eq.2.and.abs(dif_y).eq.1) then 

		b=30

		call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)
	
	elseif(tas(x1+1,y1+1).eq.30.and.(tas(x2+1,y2+1).eq.0.or.tas(x2+1,y2+1).le.5).and.abs(dif_x).eq.1.and.abs(dif_y).eq.2) then
	
		b=30

		call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)
		
	!!! sahin hareketleri (hareket ve tas yeme birlikte)
	
	elseif(tas(x1+1,y1+1).eq.60.and.(tas(x2+1,y2+1).eq.0.or.tas(x2+1,y2+1).le.5).and.abs(dif_x).le.1.and.abs(dif_y).le.1) then 

		b=60

		call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)

	!!! Rok yapma

	elseif( x1.eq.60.and.y1.eq.60.and.x2.eq.70.and.y2.eq.70.and.tas(2,2).eq.20.and.tas(2,6).eq.60) then !!!SOLA ROK'un kosulu

		rokyon=1

		oyuncu=1

		call rok(tas,oyuncu,m,rokyon) 
		
		elseif( x1.eq.60.and.y1.eq.60.and.x2.eq.80.and.y2.eq.80.and.tas(2,9).eq.20.and.tas(2,6).eq.60) then		 !!!SAGA ROK'un kosulu
		
		rokyon=2
		
		oyuncu=1
		
		call rok(tas,oyuncu,m,rokyon)
	
	else

		print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

		goto 23
	
	endif

	print*

	oyuncu=1

	call sahkontrol(m,tas,a,b,oyuncu,sahdurum)

	!print*, " 1. oyuncunu sahi", a, b, "koordinatinda " !!! programi konrtol icin yazildi silinebilir

	if(sahdurum.eq.3) then
		
		print*, " HEMEN HAMLENI DUZELT! 1. OYUNCUNUN SAHI TEHLIKE ALTINDA! "

		tas(x1+1,y1+1)=tas(x2+1,y2+1)

		tas(x2+1,y2+1)=ichange

		say=say+1
		
		if(say.eq.3) then 
		
		kazanan=2

		goto 200
		
		endif
		 
		goto 23
		
	endif 

	oyuncu=2

	call sahkontrol(m,tas,a,b,oyuncu,sahdurum)

	if(sahdurum.eq.3) print*, "2. OYUNCUNUN SAHI TEHLIKE ALTINDA!"

	say=0

	goto 25

	!!!
	!!! 2. oyuncu hareketleri	
	!!!
	!!! piyonun hareketleri

22	if(tas(x1+1,y1+1).eq.1.and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.-1.and.dif_y.eq.0) then !!!duseyde hareket

		b=1

		call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)

	elseif(tas(x1+1,y1+1).eq.1.and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.-2.and.dif_y.eq.0) then !!! duseyde 2 kare hareket			

		if(x1.eq.7) then

			b=1

			call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)

		else

			print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

			goto 23

		endif

	elseif(tas(x1+1,y1+1).eq.1.and.tas(x2+1,y2+1).ge.10.and.tas(x2+1,y2+1).ne.0.and.dif_x.eq.-1.and.abs(dif_y).eq.1) then !!!tas yeme

		b=1

		call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)


	   !!! vezir ve kalenin hareketleri

	elseif((tas(x1+1,y1+1).eq.2.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).eq.0.and.dif_y.eq.0.and.x1+1.lt.x2+1) then !!!duseyde hareket asagi

		 do i=1,dif_x
		
			if(tas(x1+1+i,y1+1).eq.0) then
			
			else
			
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23

			endif
			
		enddo
		
		b=2

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.2.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).eq.0.and.dif_y.eq.0) then !!!duseyde hareket yukari

		 do i=1,abs(dif_x)
		
			if(tas(x1+1-i,y1+1).eq.0) then

			else
			
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23

			endif
			
		enddo
		
		b=2

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.2.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.0.and.y1+1.lt.y2+2) then !!!yatay hareket saga

		do i=1,dif_y

			if(tas(x1+1,y1+1+i).eq.0) then
			
			else 
				
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "
				
				goto 23
				
			endif
			
		enddo			

		b=2

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.2.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.0) then !!! yatay hareket sola

		do i=1,abs(dif_y)

			if(tas(x1+1,y1+1-i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23

			endif

		enddo

		b=2

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.2.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).ge.10.and.dif_y.eq.0.and.x1+1.lt.x2+1) then !!! duseyde tas yeme asagi

		do i=1,dif_x-1
		
			if(tas(x1+1+i,y1+1).eq.0) then
			
			else
			
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23

			endif
			
		enddo
		
		b=2

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.2.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).ge.10.and.dif_y.eq.0) then !!! duseyde tas yeme yukari

		do i=1,abs(dif_x)-1
		
			if(tas(x1+1-i,y1+1).eq.0) then
			
			else
			
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23

			endif
			
		enddo
		
		b=2

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)


	elseif((tas(x1+1,y1+1).eq.2.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).ge.10.and.dif_x.eq.0.and.y1+1.lt.y2+1) then !!!yatay tas yeme saga

		do i=1,dif_y-1

			if(tas(x1+1,y1+1+i).eq.0) then
			
			else 
				
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "
				
				goto 23
				
			endif
			
		enddo			

		b=2

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.2.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).ge.10.and.dif_x.eq.0) then !!! yatay tas yeme sola

		do i=1,abs(dif_y)-1

			if(tas(x1+1,y1+1-i).eq.0) then
			
			else 
				
				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "
				
				goto 23
				
			endif
			
		enddo			

		b=2

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	!!!
	!!! vezir ve filin hareketleri
	!!!

	elseif((tas(x1+1,y1+1).eq.4.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.dif_y.and.x1+1.lt.x2+1) then !!!y=-x boyunca hareket asagi

		do i=1,dif_x

			if(tas(x1+1+i,y1+1+i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=4

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)


	elseif((tas(x1+1,y1+1).eq.4.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).eq.0.and.dif_x.eq.dif_y) then !!!y=-x boyunca hareket yukari

		do i=1,abs(dif_x)

			if(tas(x1+1-i,y1+1-i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=4

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)


	elseif((tas(x1+1,y1+1).eq.4.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).eq.0.and.dif_x.lt.0.and.dif_y.gt.0) then !!!y=x boyunca hareket yukari

		do i=1,dif_y

			if(tas(x1+1-i,y1+1+i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=4

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.4.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).eq.0.and.dif_x.gt.0.and.dif_y.lt.0) then !!!y=x boyunca hareket asagi

		do i=1,dif_x

			if(tas(x1+1+i,y1+1-i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=4

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)


	elseif((tas(x1+1,y1+1).eq.4.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).ge.10.and.dif_x.eq.dif_y.and.x1+1.lt.x2+1) then !!!y=-x boyunca tas yeme asagi

		do i=1,dif_x-1

			if(tas(x1+1+i,y1+1+i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=4

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.4.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).ge.10.and.dif_x.eq.dif_y) then !!!y=-x boyunca tas yeme yukari

		do i=1,abs(dif_x)-1

			if(tas(x1+1-i,y1+1-i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=4

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.4.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).ge.10.and.dif_x.lt.0.and.dif_y.gt.0) then !!!y=x boyunca tas yeme yukari

		do i=1,dif_y-1

			if(tas(x1+1-i,y1+1+i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=4

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	elseif((tas(x1+1,y1+1).eq.4.or.tas(x1+1,y1+1).eq.5).and.tas(x2+1,y2+1).ge.10.and.dif_x.gt.0.and.dif_y.lt.0) then !!!y=x boyunca tas yeme asagi

		do i=1,dif_x-1

			if(tas(x1+1+i,y1+1-i).eq.0) then

			else

				print*, " GIRDIGINIZ HAMLE YANLIS, LUTFEN KONTROL EDIN! "

				goto 23
			
			endif

		enddo

		b=4

		c=5
	
		call tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	!!! atin hareketleri (hareket ve tas yeme birlikte)

	elseif(tas(x1+1,y1+1).eq.3.and.(tas(x2+1,y2+1).eq.0.or.tas(x2+1,y2+1).ge.10).and.abs(dif_x).eq.2.and.abs(dif_y).eq.1) then 

		b=3

		call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)
	
	elseif(tas(x1+1,y1+1).eq.3.and.(tas(x2+1,y2+1).eq.0.or.tas(x2+1,y2+1).ge.10).and.abs(dif_x).eq.1.and.abs(dif_y).eq.2) then
	
		b=3

		call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)

	!!! Sahin hareketleri (hareket ve tas yeme birlikte)
	
	elseif(tas(x1+1,y1+1).eq.6.and.(tas(x2+1,y2+1).eq.0.or.tas(x2+1,y2+1).ge.10).and.abs(dif_x).le.1.and.abs(dif_y).le.1) then  

		b=6

		call  tasdegistirme2(x1,y1,x2,y2,b,tas,m)

	!!! Rok yapma

	elseif( x1.eq.60.and.y1.eq.60.and.x2.eq.70.and.y2.eq.70.and.tas(9,2).eq.2.and.tas(9,6).eq.6) then !!!SOLA ROK'un kosulu

		rokyon=1

		oyuncu=2

		call rok(tas,oyuncu,m,rokyon) 
		
	elseif( x1.eq.60.and.y1.eq.60.and.x2.eq.80.and.y2.eq.80.and.tas(9,9).eq.2.and.tas(9,6).eq.6) then !!!SAGA ROK'un kosulu
		
		rokyon=2
		
		oyuncu=2
		
		call rok(tas,oyuncu,m,rokyon)

	else 		
			
		print*, " girdiginiz hamle yanlis "
				
		goto 23
				
	endif

	oyuncu=2

	call sahkontrol(m,tas,a,b,oyuncu,sahdurum)

	!print*, " 1. oyuncunu sahi", a, b, "koordinatinda " !!! programi konrtol icin yazildi silinebilir

	if(sahdurum.eq.3) then
		
		print*, " HEMEN HAMLENI DUZELT! 2. OYUNCUNUN SAHI TEHLIKE ALTINDA! "

		tas(x1+1,y1+1)=tas(x2+1,y2+1)

		tas(x2+1,y2+1)=ichange

		say=say+1
		
		if(say.eq.3) then 
		
			kazanan=1

			goto 200

		endif
		
		goto 23

		
	endif 

	oyuncu=1

	call sahkontrol(m,tas,a,b,oyuncu,sahdurum)

	if(sahdurum.eq.3) print*, "1. OYUNCUNUN SAHI TEHLIKE ALTINDA!"
	
	say=0

25	do i=1,m				

		print*
							 
		write(*,51) (tas(i,j), j=1,m )

		print*

	enddo

	enddo

200	if(kazanan.eq.1) print*, " TEBRIKLER 1. OYUNCU! SEN KAZANDIN!!! "

	if(kazanan.eq.2) print*, " TEBRIKLER 2. OYUNCU! SEN KAZANDIN!!! "

	pause

	end program

	!!!
	!!! altprogramlar
	!!!

	!!!altprogram 1// kale, fil ve vezirin yer degistirmesi
	
	subroutine tasdegistirme1(tas,m,x1,y1,x2,y2,b,c)

	integer tas(m,m), x1,y1,x2,y2,b,c

	if(tas(x1+1,y1+1).eq.b) then

			tas(x1+1,y1+1)=0

			tas(x2+1,y2+1)=b

		else

			tas(x1+1,y1+1)=0

			tas(x2+1,y2+1)=c

		endif

	return

	end

	!!!altprogram2// piyon, sah ve atin yer degistirmesi

	subroutine  tasdegistirme2(x1,y1,x2,y2,b,tas,m)

	integer x1,y1,x2,y2,b,tas(m,m)

	tas(x1+1,y1+1)=0

	tas(x2+1,y2+1)=b

	return

	end

	!!!altprogram3/// sah cekme

	subroutine sahkontrol(m,tas,a,b,oyuncu,sahdurum)

	integer deger,a,b,tas(m,m),x,y,oyuncu,sah,piyon,kale,at,fil,vezir, sahdurum

	sahdurum=0

	if(oyuncu.eq.2) then
		
		sah=6
		piyon=10
		kale=20
		at=30
		fil=40
		vezir=50

	elseif(oyuncu.eq.1) then
	
		sah=60
		piyon=1
		kale=2
		at=3
		fil=4
		vezir=5
		
	endif
		
	do x=2,m
	
		do y=2,m
		
			deger=tas(x,y)
			
			if(deger.eq.sah) then

				a=x

				b=y

				dif1=9-a

				dif2=a-2

				dif3=9-b

				dif4=b-2

				if(oyuncu.eq.1) goto 100

				if(oyuncu.eq.2) goto 108

				

			endif
			
		enddo
		
	enddo

	

	!!!1. oyuncu icin kontrol
	!!! dusey dogrultuda asagi kontrol

100	do i=1,dif1

		if(dif1.eq.0) goto 101
		
		if(tas(a+i,b).eq.0) then

		elseif(tas(a+i,b).eq.piyon.or.tas(a+i,b).eq.fil.or.tas(a+i,b).eq.at.or.tas(a+i,b).ge.10) then

			

			goto 101

		elseif(tas(a+i,b).eq.kale.or.tas(a+i,b).eq.vezir) then

			sahdurum=3

			

			goto 101
			
		endif
			
	enddo

	!!!dusey dogrultuda yukari kontrol

101	do i=1,dif2

		if(dif2.eq.0) goto 102
		
		if(tas(a-i,b).eq.0) then

		elseif(tas(a-i,b).eq.piyon.or.tas(a-i,b).eq.fil.or.tas(a-i,b).eq.at.or.tas(a-i,b).ge.10) then

			

			goto 102

		elseif(tas(a-i,b).eq.kale.or.tas(a-i,b).eq.vezir) then

			sahdurum=3

		

			goto 102
			
		endif
			
	enddo

	!!!yatay dogrultuda saga kontrol

102	do i=1,dif3

		if(dif3.eq.0) goto 103
		
		if(tas(a,b+i).eq.0) then

		elseif(tas(a,b+i).eq.piyon.or.tas(a,b+i).eq.fil.or.tas(a,b+i).eq.at.or.tas(a,b+i).ge.10) then

		

			goto 103

		elseif(tas(a,b+i).eq.kale.or.tas(a,b+i).eq.vezir) then

			sahdurum=3

			

			goto 103
			
		endif
			
	enddo

	!!!yatay dogrultuda sola kontrol

103	do i=1,dif4

		if(dif4.eq.0) goto 104
		
		if(tas(a,b-i).eq.0) then

		elseif(tas(a,b-i).eq.piyon.or.tas(a,b-i).eq.fil.or.tas(a,b-i).eq.at.or.tas(a,b-i).ge.10) then

		

			goto 104

		elseif(tas(a,b-i).eq.kale.or.tas(a,b-i).eq.vezir) then

			sahdurum=3

			

			goto 104
			
		endif
			
	enddo

	!!! x=y dogrusunda asagi

104	if(dif1.ge.dif4) then

		do i=1,dif4
		
			if(tas(a+i,b-i).eq.0) then

			elseif(tas(a+i,b-i).eq.kale.or.tas(a+i,b-i).eq.at.or.tas(a+i,b-i).ge.10) then

				

				goto 105

			elseif(i.gt.1.and.tas(a+i,b-i).eq.piyon) then
			
				goto 105 

			elseif(i.eq.1.and.tas(a+i,b-i).eq.piyon) then

				sahdurum=3

				
				goto 105

			elseif(tas(a+i,b-i).eq.fil.or.tas(a+i,b-i).eq.vezir) then

				sahdurum=3

				

				goto 105
					
			endif
			
		enddo

	else

		do i=1,dif1

			if(tas(a+i,b-i).eq.0) then

			elseif(tas(a+i,b-i).eq.kale.or.tas(a+i,b-i).eq.at.or.tas(a+i,b-i).ge.10) then

				

				goto 105

			elseif(i.gt.1.and.tas(a+i,b-i).eq.piyon) then
			
				goto 105 

			elseif(i.eq.1.and.tas(a+i,b-i).eq.piyon) then

				sahdurum=3

			

				goto 105

			elseif(tas(a+i,b-i).eq.fil.or.tas(a+i,b-i).eq.vezir) then

				sahdurum=3

				

				goto 105
			
			endif
			
		enddo

	endif

	!!!x=y dogrusunda yukari

105	if(dif2.ge.dif3) then

		do i=1,dif3
		
			if(tas(a-i,b+i).eq.0) then

			elseif(tas(a-i,b+i).eq.piyon.or.tas(a-i,b+i).eq.kale.or.tas(a-i,b+i).eq.at.or.tas(a-i,b+i).ge.10) then

				

				goto 106

			elseif(tas(a-i,b+i).eq.fil.or.tas(a-i,b+i).eq.vezir) then

				sahdurum=3

				

				goto 106
			
			endif
			
		enddo

	else

		do i=1,dif2

			if(tas(a-i,b+i).eq.0) then

			elseif(tas(a-i,b+i).eq.piyon.or.tas(a-i,b+i).eq.kale.or.tas(a-i,b+i).eq.at.or.tas(a-i,b+i).ge.10) then

			

				goto 106

			elseif(tas(a-i,b+i).eq.fil.or.tas(a-i,b+i).eq.vezir) then

				sahdurum=3

			

				goto 106
			
			endif
			
		enddo

	endif

	!!!x=-y dogrusunda asagi

106	if(dif1.ge.dif3) then

		do i=1,dif3
		
			if(tas(a+i,b+i).eq.0) then

			elseif(tas(a+i,b+i).eq.kale.or.tas(a+i,b+i).eq.at.or.tas(a+i,b+i).ge.10) then

				goto 107

			elseif(i.gt.1.and.tas(a+i,b+i).eq.piyon) then
			
				goto 107 

			elseif(i.eq.1.and.tas(a+i,b+i).eq.piyon) then

				sahdurum=3

			

				goto 107

			elseif(tas(a+i,b+i).eq.fil.or.tas(a+i,b+i).eq.vezir) then

				sahdurum=3

		

				goto 107
			
			endif
			
		enddo

	else

		do i=1,dif1

			if(tas(a+i,b+i).eq.0) then

			elseif(tas(a+i,b+i).eq.kale.or.tas(a+i,b+i).eq.at.or.tas(a+i,b+i).ge.10) then

			
				goto 107

			elseif(i.gt.1.and.tas(a+i,b+i).eq.piyon) then
			
				goto 107 

			elseif(i.eq.1.and.tas(a+i,b+i).eq.piyon) then

				sahdurum=3

			

				goto 107

			elseif(tas(a+i,b+i).eq.fil.or.tas(a+i,b+i).eq.vezir) then

				sahdurum=3

		
				goto 107
			
			endif
			
		enddo

	endif

	!!! x=-y dogrusunda yukari

107	if(dif2.ge.dif4) then

		do i=1,dif4
		
			if(tas(a-i,b-i).eq.0) then

			elseif(tas(a-i,b-i).eq.piyon.or.tas(a-i,b-i).eq.kale.or.tas(a-i,b-i).eq.at.or.tas(a-i,b-i).ge.10) then

			

				goto 116

			elseif(tas(a-i,b-i).eq.fil.or.tas(a-i,b-i).eq.vezir) then

				sahdurum=3

			

				goto 116
			
			endif
			
		enddo

	else

		do i=1,dif2

			if(tas(a-i,b-i).eq.0) then

			elseif(tas(a-i,b-i).eq.piyon.or.tas(a-i,b-i).eq.kale.or.tas(a-i,b-i).eq.at.or.tas(a-i,b-i).ge.10) then

			

				goto 116

			elseif(tas(a-i,b-i).eq.fil.or.tas(a-i,b-i).eq.vezir) then

				sahdurum=3

			

				goto 116
			
			endif
			
		enddo

	endif


	!!!	2. oyuncu icin konrtol
	!!! dusey dogrultuda asagi kontrol

108	do i=1,dif1

		if(dif1.eq.0) goto 109
		
		if(tas(a+i,b).eq.0) then

		elseif(tas(a+i,b).eq.piyon.or.tas(a+i,b).eq.fil.or.tas(a+i,b).eq.at.or.tas(a+i,b).le.5) then

		

			goto 109

		elseif(tas(a+i,b).eq.kale.or.tas(a+i,b).eq.vezir) then

			sahdurum=3

		

			goto 109
			
		endif
			
	enddo

	!!!dusey dogrultuda yukari kontrol

109	do i=1,dif2

		if(dif2.eq.0) goto 110
		
		if(tas(a-i,b).eq.0) then

		elseif(tas(a-i,b).eq.piyon.or.tas(a-i,b).eq.fil.or.tas(a-i,b).eq.at.or.tas(a-i,b).le.5) then

		

			goto 110

		elseif(tas(a-i,b).eq.kale.or.tas(a-i,b).eq.vezir) then

			sahdurum=3

		

			goto 110
			
		endif
			
	enddo

	!!!yatay dogrultuda saga kontrol

110	do i=1,dif3

		if(dif3.eq.0) goto 111
		
		if(tas(a,b+i).eq.0) then

		elseif(tas(a,b+i).eq.piyon.or.tas(a,b+i).eq.fil.or.tas(a,b+i).eq.at.or.tas(a,b+i).le.5) then

	

			goto 111

		elseif(tas(a,b+i).eq.kale.or.tas(a,b+i).eq.vezir) then

			sahdurum=3

		

			goto 111
			
		endif
			
	enddo

	!!!yatay dogrultuda sola kontrol

111	do i=1,dif4

		if(dif4.eq.0) goto 112
		
		if(tas(a,b-i).eq.0) then

		elseif(tas(a,b-i).eq.piyon.or.tas(a,b-i).eq.fil.or.tas(a,b-i).eq.at.or.tas(a,b-i).le.5) then

	

			goto 112

		elseif(tas(a,b-i).eq.kale.or.tas(a,b-i).eq.vezir) then

			sahdurum=3

	

			goto 112
			
		endif
			
	enddo

	!!! x=y dogrusunda asagi

112	if(dif1.ge.dif4) then

		do i=1,dif4
		
			if(tas(a+i,b-i).eq.0) then

			elseif(tas(a+i,b-i).eq.piyon.or.tas(a+i,b-i).eq.kale.or.tas(a+i,b-i).eq.at.or.tas(a+i,b-i).le.5) then

			

				goto 113

			elseif(tas(a+i,b-i).eq.fil.or.tas(a+i,b-i).eq.vezir) then

				sahdurum=3

			

				goto 113
			
			endif
			
		enddo

	else

		do i=1,dif1

			if(tas(a+i,b-i).eq.0) then

			elseif(tas(a+i,b-i).eq.piyon.or.tas(a+i,b-i).eq.kale.or.tas(a+i,b-i).eq.at.or.tas(a+i,b-i).le.5) then

		

				goto 113

			elseif(tas(a+i,b-i).eq.fil.or.tas(a+i,b-i).eq.vezir) then

				sahdurum=3

			

				goto 113
			
			endif
			
		enddo

	endif

	!!!x=y dogrusunda yukari

113	if(dif2.ge.dif3) then

		do i=1,dif3
		
			if(tas(a-i,b+i).eq.0) then

			elseif(tas(a-i,b+i).eq.kale.or.tas(a-i,b+i).eq.at.or.tas(a-i,b+i).le.5) then

			

				goto 114

			elseif(i.gt.1.and.tas(a-i,b+i).eq.piyon) then
			
				goto 114 

			elseif(i.eq.1.and.tas(a-i,b+i).eq.piyon) then

				sahdurum=3

			

				goto 114

			elseif(tas(a-i,b+i).eq.fil.or.tas(a-i,b+i).eq.vezir) then

				sahdurum=3

			

				goto 114
			
			endif
			
		enddo

	else

		do i=1,dif2

			if(tas(a-i,b+i).eq.0) then

			elseif(tas(a-i,b+i).eq.kale.or.tas(a-i,b+i).eq.at.or.tas(a-i,b+i).le.5) then

			

				goto 114

			elseif(i.gt.1.and.tas(a-i,b+i).eq.piyon) then
			
				goto 114 

			elseif(i.eq.1.and.tas(a-i,b+i).eq.piyon) then

				sahdurum=3

			

				goto 114

			elseif(tas(a-i,b+i).eq.fil.or.tas(a-i,b+i).eq.vezir) then

				sahdurum=3

			

				goto 114
			
			endif
			
		enddo

	endif

	!!!x=-y dogrusunda asagi

114	if(dif1.ge.dif3) then

		do i=1,dif3
		
			if(tas(a+i,b+i).eq.0) then

			elseif(tas(a+i,b+i).eq.piyon.or.tas(a+i,b+i).eq.kale.or.tas(a+i,b+i).eq.at.or.tas(a+i,b+i).le.5) then

			
				goto 115

			elseif(tas(a+i,b+i).eq.fil.or.tas(a+i,b+i).eq.vezir) then

				sahdurum=3

			

				goto 115
			
			endif
			
		enddo

	else

		do i=1,dif1

			if(tas(a+i,b+i).eq.0) then

			elseif(tas(a+i,b+i).eq.piyon.or.tas(a+i,b+i).eq.kale.or.tas(a+i,b+i).eq.at.or.tas(a+i,b+i).le.5) then

			

				goto 115

			elseif(tas(a+i,b+i).eq.fil.or.tas(a+i,b+i).eq.vezir) then

				sahdurum=3

			

				goto 115
			
			endif
			
		enddo

	endif

	!!! x=-y dogrusunda yukari

115	if(dif2.ge.dif4) then

		do i=1,dif4
		
			if(tas(a-i,b-i).eq.0) then

			elseif(tas(a-i,b-i).eq.kale.or.tas(a-i,b-i).eq.at.or.tas(a-i,b-i).le.5) then

			

				goto 116

			elseif(i.gt.1.and.tas(a-i,b-i).eq.piyon) then
			
				goto 116 

			elseif(i.eq.1.and.tas(a-i,b-i).eq.piyon) then

				sahdurum=3

			
				goto 116

			elseif(tas(a-i,b-i).eq.fil.or.tas(a-i,b-i).eq.vezir) then

				sahdurum=3

			

				goto 116
			
			endif
			
		enddo

	else

		do i=1,dif2

			if(tas(a-i,b-i).eq.0) then

			elseif(tas(a-i,b-i).eq.kale.or.tas(a-i,b-i).eq.at.or.tas(a-i,b-i).le.5) then

			

				goto 116

			elseif(i.gt.1.and.tas(a-i,b-i).eq.piyon) then
			
				goto 116 

			elseif(i.eq.1.and.tas(a-i,b-i).eq.piyon) then

				sahdurum=3

				

				goto 116

			elseif(tas(a-i,b-i).eq.fil.or.tas(a-i,b-i).eq.vezir) then

				sahdurum=3

			
				goto 116
			
			endif
			
		enddo

	endif

	!!! at kontrolu-butun oyuncular icin ortak

116	if(tas(a+2,b+1).eq.at.and.a+2.ne.11) then 

	

		sahdurum=3

		goto 117

	elseif(tas(a+2,b-1).eq.at) then

		

		sahdurum=3

		goto 117

	elseif(tas(a-2,b+1).eq.at) then

	

		sahdurum=3

		goto 117
	
	elseif(tas(a-2,b-1).eq.at) then

		

		sahdurum=3

		goto 117

	elseif(tas(a+1,b-2).eq.at) then

		

		sahdurum=3

		goto 117

	elseif(tas(a+1,b+2).eq.at) then

	

		sahdurum=3

		goto 117

	elseif(tas(a-1,b-2).eq.at.and.a-1.ne.1) then

		

		sahdurum=3

		goto 117

	elseif(tas(a-1,b+2).eq.at) then

	

		sahdurum=3

		goto 117

117	endif

	return

	end

	!!!Rok

	subroutine rok (tas,oyuncu,m,rokyon)  

	integer kale,tas(m,m),oyuncu,sah,rokyon

	if(oyuncu.eq.1) then
		
		sah=60
		kale=20

		if(rokyon.eq.2) then

			tas(2,8) = sah

			tas(2,7) = kale

			tas(2,9)=0

			tas(2,6)=0

		elseif(rokyon.eq.1) then

			tas(2,3) = sah

			tas(2,4) = kale

			tas(2,2)=0
		
			tas(2,6)=0

		endif
		
	elseif(oyuncu.eq.2) then
	
		sah=6
		kale=2

		if(rokyon.eq.2) then

			tas(9,8) = sah

			tas(9,7) = kale

			tas(9,9)=0

			tas(9,6)=0

		elseif(rokyon.eq.1) then

			tas(9,3) = sah
		
			tas(9,4) = kale

			tas(9,2)=0

			tas(9,6)=0

		endif

	endif
		
	return

	end
	
