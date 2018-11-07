      PROGRAM STFND
C-----------------------------------------------------------------------
C! Task to find stars in an image and generate an ST table.
C# EXT-util Map-util Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2006, 2009, 2015
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   Task to find stars in an image and generate an ST table.
C    INPUTS:   (from AIPS)
C              USERID   R   user number, 0 means use logon user
C                       number, 32000 means any user can be accessed.
C              INNAME   R(3)   name of primary file.
C              INCLASS  R(2)   class of primary file.
C              INSEQ    R   sequence number of primary file.
C              INDISK   R    disk volume number. 0 means try all.
C              OUTVERS  R    ST file version number.
C              BLC      R(7)  Bottom corner to search for stars
C              TRC      R(7)  Top    corner to search for stars
C              CPARM    R(10) User parameters
C-----------------------------------------------------------------------
C                                       Max Number Columns, Label Length
      INTEGER MXSTCL, MXSTLB
      PARAMETER (MXSTCL=7, MXSTLB=24)
      CHARACTER PRGNAM*6, NAMIN*12, CLSIN*6, TYPIN*2, STFILE*48,
     *   CHTM12*12, TYPES(2)*8
      HOLLERITH XNAMIN(3), XCLSIN(2)
      REAL      DSKIN, SEQIN, XOUVER, BLC(7), TRC(7), CPARM(10)
      INTEGER  I, IWBUFF(256), IMFIND, IMLUN, IERR, IRETCD, ISEQ,
     *   INPRMS, ISLOT, IUSER, IVOL, IROUND, SLUN, J, BUF(512), OUVER,
     *   ICTYPE(MXSTCL), IDIV, IRNO
      LOGICAL   T, F, SAVE, EQUAL, QUICK
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /INPARM/ XNAMIN, XCLSIN, SEQIN, DSKIN, XOUVER, BLC, TRC,
     *   CPARM
      DATA IMLUN, SLUN /16,29/
      DATA PRGNAM /'STFND '/
      DATA TYPIN /'  '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Initialize the IO parameters.
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      SAVE = F
C                                       Get input values from AIPS.
      INPRMS = 8+10+14
      IRETCD = 0
      CALL GTPARM (PRGNAM, INPRMS, QUICK, XNAMIN, IWBUFF, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         IRETCD = 8
         END IF
      IF (QUICK) CALL RELPOP (IRETCD, IWBUFF, IERR)
      IF (IRETCD.NE.0) GO TO 990
      IRETCD = 8
C                                       Hollerith -> char
      CALL H2CHR (12, 1, XNAMIN, NAMIN)
      CALL H2CHR (6, 1, XCLSIN, CLSIN)
C
      ISEQ = IROUND (SEQIN)
      IVOL = IROUND (DSKIN)
      IUSER = NLUSER
      OUVER = IROUND (XOUVER)
C                                       Open map file & get header.
      CALL MAPOPN ('HDWR', IVOL, NAMIN, CLSIN, ISEQ, TYPIN, IUSER,
     *   IMLUN, IMFIND, ISLOT, CATBLK, IWBUFF, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Fill in defaults in PARMS
      DSKIN = IVOL
C                                       If sending Pixels
      IF (CPARM(10).LE.-1.0) THEN
C                                       Save Old Types
         CALL H2CHR (4, 1, CATH(KHCTP), TYPES(1))
         CALL H2CHR (4, 1, CATH(KHCTP+2), TYPES(2))
C                                       Tell STOPEN: send pixesl
         CALL CHR2H (4, '    ', 1, CATH(KHCTP))
         CALL CHR2H (4, '    ', 1, CATH(KHCTP+2))
C                                       End if sending pixles
         END IF
C                                       Open/Init Star Table
      CALL STOPEN (SLUN, IDIV, IVOL, ISLOT, OUVER, ICTYPE, BUF, IERR)
C                                       If sending Pixels
      IF (CPARM(10).LE.-1.0) THEN
C                                       Copy Old Types back
         CALL CHR2H (4, TYPES(1), 1, CATH(KHCTP))
         CALL CHR2H (4, TYPES(2), 1, CATH(KHCTP+2))
C                                       End if sending pixles
         END IF
      IRNO = 0
C                                       Read through map find stars
      CALL SENDMA (IMLUN, IMFIND, IRNO, BLC, TRC, CPARM, BUF, IERR)
C                                       CLose table, add history
      CALL STCLOS (IRNO, IVOL, ISLOT, OUVER, BUF, IERR)
C                                       IF no I/O error, jump to end
      IF (IERR.EQ.0) THEN
C                                       Mark ST table as OK
         IRETCD = 0
         GO TO 970
         END IF
C                                       ERRORS: Kill the ST table
      CALL ZCLOSE (SLUN, BUF(82), IERR)
      CALL H2CHR (24, 1, BUF(17), STFILE)
      CALL ZDESTR (IVOL, STFILE, IERR)
C                                       STOPEN -> TABINI does FXHDEX
      DO 965 I = 1,KIEXTN
         J = I - 1
         CALL H2CHR (2, 1, CATH(KHEXT+J), CHTM12)
         EQUAL = CHTM12(1:2) .EQ. 'ST'
         IF (.NOT.EQUAL) GO TO 965
            IF (OUVER.EQ.CATBLK(KIVER+J)) CATBLK(KIVER+J) =
     *         CATBLK(KIVER+J) - 1
            SAVE = T
            GO TO 970
 965     CONTINUE
 970  CONTINUE
C                                       Close map file.
      CALL MAPCLS ('WRIT', IVOL, ISLOT, IMLUN, IMFIND, CATBLK, T,
     *   IWBUFF, IERR)
C
 990  CALL DIETSK (IRETCD, QUICK, IWBUFF)
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR GETTING PARAMETERS FROM AIPS. GTPARM ERR =',I5)
      END
      SUBROUTINE SENDMA (IMLUN, IMFIND, IRNO, BLC, TRC, CPARM, BUF,
     *   IRET)
C-----------------------------------------------------------------------
C   SENDMA sends image one row at a time to the user supplied
C   routine and then writes the modified data.
C   Input:
C     BLC     R(7)    boundary of region to search for stars
C     TRC     R(7)    boundary of region to search for stars
C     CPARM   R(10)   Star selection critera
C   Input/output:
C     IRNO    I       Number of stars written
C     BUF     I       I/O buffer for ST table
C   Output:
C      IRET   I       Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IMLUN, IMFIND, IRNO, BUF(*), IRET
      REAL      BLC(7), TRC(7), CPARM(10)
C
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   IROUND, NYI, NXI, WINI(4), JBUFSZ, BOI, LIM2, LIM3,
     *   LIM4, LIM5, LIM6, LIM7, I2, I3, I4, I5, I6, I7, IPOS(7),
     *   CORN(7), BOTEMP, IBIND, LIM1, DIDRET
      REAL      BUFF1(MABFSS)
      LOGICAL   T
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DLOC.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
      JBUFSZ = 2 * MABFSS
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATBLK(KIDIM), CATBLK(KINAX), BLC, TRC, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Setup for I/O
      NXI = CATBLK(KINAX)
      NYI = CATBLK(KINAX+1)
      WINI(1) = IROUND (BLC(1))
      WINI(2) = IROUND (BLC(2))
      WINI(3) = IROUND (TRC(1))
      WINI(4) = IROUND (TRC(2))
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4) = I4
                  DO 300 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3) = I3
C                                       Init. fiile
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1099) IRET
            GO TO 990
            END IF
C                                       Prepare for XYVAL calls
         LOCNUM = 1
         CALL SETLOC (IPOS(3), T)
C                                       Prepare to read map
         BOI = BOTEMP + 1
         CALL MINIT ('READ', IMLUN, IMFIND, NXI, NYI, WINI, BUFF1,
     *      JBUFSZ, BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 990
            END IF
C                                       For all Rows loop
         DO 220 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
C                                       Read.
            CALL MDISK ('READ', IMLUN, IMFIND, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                       Find in stars
            CALL STLOC ( IPOS, LIM1, BUFF1(IBIND), LIM2, CPARM, IRNO,
     *         BUF, IRET)
            DIDRET = IRET
            IF (DIDRET.NE.0) GO TO 750
C                                       End of For all rows loop
 220        CONTINUE
 300     CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Final call to functions
 750  IPOS(1) = -2
      CALL STLOC ( IPOS, LIM1, BUFF1(IBIND), LIM2, CPARM,
     *   IRNO, BUF, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180) IRET
         GO TO 990
         END IF
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1099 FORMAT ('SENDMA: COMOFF ERROR',I3)
 1100 FORMAT ('SENDMA: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('SENDMA: ',A,' ERROR',I3)
 1180 FORMAT ('SENDMA: DIDDLE ERROR',I3)
      END
      SUBROUTINE STLOC (IPOS, NPX, Z, NRW, CPARM, IRNO, BUF, IERR)
C-----------------------------------------------------------------------
C STLOC locates a star candiate on a row of an image.  Star candiates
C from previous rows are merged on the fly.  The stars are located by
C finding a series of pixels with values greater that the threshhold.
C The series is called a "RUN".  After merging, the Merged RUN must have
C an area greater than the area minimum
C   INPUTS:
C     IPOS      I(7)   Pixel Coordinate of first pixel in the row
C     NPX       I      number of pixels on the current row
C     Z         R(NPX) Row of the image
C     NRW       I      Number of rows in the image.
C     CPARM     R(10)  User input parameters
C   I/O
C     BUF       I(*)   I/O Buffer for ST table
C   OUTPUT
C     IERR      I      I/O error number. 0=> OK
C Written by Don Wells
C Modified by Glen Langston  92 May 31
C-----------------------------------------------------------------------
      INTEGER IPOS(7), NRW, NPX, IRNO, BUF(*), IERR
      REAL Z(*), CPARM(10)
C
      CHARACTER STRCHR*24
      INTEGER   MXRUNS
      PARAMETER (MXRUNS=3000)
      INTEGER IXY(3,MXRUNS), ITEM(MXRUNS)
      INTEGER NRL, NITEM, J, I, K, L, INNRL, JA, JAJ, INJ, DELI,
     *   IA, IB, NRLM, IAI, IBI, IAJ, IBJ, M, MXSTAR, STTYPE,
     *   MAXPIX, MAXROW, IROUND
      REAL ZS, ZSUM, XZSUM, AREA, XC, YC, XS(3,MXRUNS), THRESH,
     *   MNAREA, MXAREA, A1, A2, A3, CELLS(2)
      LOGICAL DORADC, ONCE, DBUGIT
      DOUBLE PRECISION P(6), DUMMY
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C                                      Get FBLANK from DDCH
      INCLUDE 'INCS:DDCH.INC'
      SAVE NITEM, NRL, M, MXSTAR, STTYPE, ONCE, DBUGIT, IXY, ITEM,
     *     MAXROW, MAXPIX, MNAREA, MXAREA, THRESH, A1, A2, A3, DORADC,
     *     CELLS, XS
C
      DATA NRL/0/,NITEM/0/,NRLM/0/, ONCE/.FALSE./
C-----------------------------------------------------------------------
C                                      Set start pixel number
      IB = 1
C                                      JA = current row number
      JA = IPOS(2)
C                                      On first row, report parms
      IF (.NOT. ONCE) THEN
C                                       Translate user inputs
         THRESH = CPARM(1)
C                                       If no threshold
         IF (THRESH.EQ.0.0) THEN
C                                       use 95 % of maximum
            THRESH = 0.95 * CATR(KRDMX)
            END IF
C                                       Distance between runs to merge
         M      = CPARM(2)
         IF (M.LT.1) M = 0
C                                       Min star area, no lower limit
         MNAREA = CPARM(3)
C                                       Max star area
         MXAREA = CPARM(4)
C                                       Make max, 1 big star
         IF (MXAREA.EQ.0) MXAREA = NPX*NRW/4.
C                                       Scales for star size, area
         A1     = CPARM(5)
C                                       Total count
         A2     = CPARM(6)
C                                       Size constant
         A3     = CPARM(7)
C                                       default star is sqrt area
         IF (A1.EQ.0.0 .AND. A2.EQ.0.0 .AND. A3.EQ.0.0) A1 = 1.
C                                       Set type of star
         STTYPE = IROUND (CPARM(8))
         STTYPE = MAX (1, MIN (24, STTYPE))
C                                       Limit maximum number of stars
         MXSTAR = ABS (CPARM(9))
         IF (MXSTAR.LE.0) MXSTAR = 100
C                                       Debug hook
         DBUGIT = CPARM(9) .LT. -1
C                                       Translate to RA and Dec?
         DORADC = CPARM(10).GT.-1.0
C                                       conversion from pixels to degrees
         CELLS(1) = ABS (CATR(KRCIC))
         CELLS(2) = ABS (CATR(KRCIC+1))
C                                       Tell User
         WRITE (MSGTXT,1000) NPX, NRW, MXSTAR
         CALL MSGWRT(3)
         WRITE (MSGTXT,1100) THRESH
         CALL MSGWRT(3)
         WRITE (MSGTXT,1110) MNAREA, MXAREA
         CALL MSGWRT(3)
         WRITE (MSGTXT,1130) M
         CALL MSGWRT(3)
         IF (DORADC) THEN
            MSGTXT='STLOC:  Translating Star coordinate to RA and DEC '
         ELSE
            MSGTXT='STLOC:  Returning Star coordinate in Pixels '
            END IF
         CALL MSGWRT(3)
C                                      Init Arrays
         CALL FILL (MXRUNS, 0, ITEM)
         CALL FILL (MXRUNS*3, 0, IXY)
         CALL RFILL (MXRUNS*3, 0.0, XS)
C                                      End initialize once
C                                      Maximum pixel is start + number
         MAXPIX = IB + NPX - 1
C                                      Maximum row is start + number
         MAXROW = JA + NRW - 1
         END IF
C                                      If time to flush last
      IF (IB .LT. 0 .OR. JA .GT. MAXROW) THEN
C                                      Handle case of star on last row
         JA=MAXROW+M+2
         IB=MAXPIX
C                                      jump to flush list
         GO TO 90
      END IF
C                                      Every so often print progress
      IF ((MOD(JA,100).EQ.1).OR.(JA.EQ.MAXROW)) THEN
         WRITE (MSGTXT,1200) JA
         CALL MSGWRT(3)
         END IF
C                                      Loop here for start of a run
      IA = IB
C
 10   IA = IA+1
C                                       While Row not finished
      IF (IA.GT.MAXPIX) GO TO 90
C                                       blanked pixel below threshold
      IF (Z(IA).EQ.FBLANK) GO TO 10
C                                       If pixel less than threshold
      IF (Z(IA).LT.THRESH) GO TO 10
C                                       FOUND A RUN: LOOK FOR ITS END:
      IB=IA
C                                       Do pixel statistics
      ZSUM  = Z(IB)
      XZSUM = IB*Z(IB)
 20   IB=IB+1
C                                       While not end of row loop
         IF (IB.GT.MAXPIX) GO TO 30
C                                       blanked pixels below threshold
         IF (Z(IB).EQ.FBLANK) GO TO 30
C                                       if pixel is below threshold
         IF (Z(IB).LT.THRESH) GO TO 30
         ZSUM  = ZSUM  + Z(IB)
         XZSUM = XZSUM + IB*Z(IB)
C                                       Still in a RUN find next pixel
         GO TO 20
C                                       Found end of a run, save end
 30   IB=IB-1
C                                       PUT NEW RUN AT END OF LIST:
C                                       Increase total list
      NRL=NRL+1
C                                       Check for limits
      IF (NRL.GT.MXRUNS) THEN
         MSGTXT = 'STLOC:  MAXIMUM NUMBER OF STAR CANDIDATES EXCEEDED'
         CALL MSGWRT(8)
         MSGTXT = 'STLOC:  INCREASE THRESHOLD AND/OR AREA LIMITS'
         CALL MSGWRT(8)
         IERR = 1
         GO TO 999
         END IF
C                                       record max number of runs
      NRLM = MAX (NRL,NRLM)
C                                       Record Row, start, stop columns
      IXY(1,NRL) = JA
      IXY(2,NRL) = IA
      IXY(3,NRL) = IB
C                                       Record Sum and X weighted sum
      XS(1,NRL)  = ZSUM
      XS(2,NRL) = XZSUM
      XS(3,NRL) = JA*ZSUM
C                                       Is this part of a previous run
      J=NRL
C                                       Start looking backwards
   40 J=J-1
C                                       While not all previous lists
      IF (J.LT.1) GO TO 80
      JAJ = IXY(1,J)
C                                       If previous row far from current
      IF (JAJ.LT.(JA-M-1)) GO TO 80
C                                       OVERLAP TESTS:
      IAJ = IXY(2,J)
      IBJ = IXY(3,J)
C                                       If previous near by
      IF(((IA-M).LE.IAJ).AND.(IAJ.LE.(IB+M)))GO TO 50
      IF(((IA-M).LE.IBJ).AND.(IBJ.LE.(IB+M)))GO TO 50
      IF(((IAJ-M).LE.IA).AND.(IA.LE.(IBJ+M)))GO TO 50
C                                       Else previous far jump to next
      GO TO 40
C                                       OVERLAP DETECTED, assign an item
   50 INNRL=ITEM(NRL)
C                                       IS ITEM NUMBER ALREADY SET?
      IF (INNRL.EQ.0) THEN
C                                       NO, SO USE NUMBER OF OVERLAPPED RUN:
         ITEM(NRL) = ITEM(J)
C                                       go look for next overlap
         GO TO 40
         END IF
C                                       Else item number set
      INJ=ITEM(J)
C                                       If part of this item, go look
C                                       for an overlap
      IF (INNRL.EQ.INJ) GO TO 40
C-----OOPS, NOT EQUAL, SO THE TWO OBJECT DESCRIPTIONS MUST BE MERGED.
C     SUPPRESS OVERLAPPED OBJECT NUMBER:
C                                       for all runs
      DO 70 I=1,NRL
C                                       if part of old item, make new
         IF (ITEM(I).EQ.INJ) ITEM(I) = ITEM(INNRL)
   70    CONTINUE
C                                       Go look for next overlap
      GO TO 40
C                                       SEARCH COMPLETE
   80 INNRL=ITEM(NRL)
C                                       IS ITEM NUMBER SET?
      IF (INNRL .EQ. 0) THEN
C                                       start a new item
         NITEM = NITEM + 1
         ITEM(NRL) = NITEM
         END IF
C                                       Item finished, go find next
      IA = IB + 1
C                                       go look for start of next run
      GO TO 10
C                                       Jump here on end of row
   90 CONTINUE
C                                       Row is finished. If previous row
      IF (ONCE) THEN
C                                       For all runs loop
         J=NRL+1
C                                       do while not all other objects
  100    J=J-1
C                                       If past begin of run list, exit
         IF (J.LT.1) GO TO 120
C                                       If object not done, get next object
         IF (IXY(1,J).LT.0) GO TO 100
C                                       set row of previous run
         JAJ = IXY(1,J)
C                                       if object close to row, no flag
         IF (JAJ.LT.(JA-1-M)) GO TO 100
C                                       Else near row, flag this item
         INJ=ITEM(J)
C                                       FLAG THE OBJECT # OF RUN J:
            DO 110 I=1,NRL
C                                       Objects are complete when row<0
            IF (IXY(1,I).GT.0) THEN
               IF (ITEM(I).EQ.INJ) IXY(1,I)=-IXY(1,I)
               END IF
  110       CONTINUE
C                                       End of all runs loop
         GO TO 100
C                                       Dump all unflagged objects
  120    J=0
C                                       Start all runs loop
  130    J=J+1
C                                       exit if past end all runs
  140    IF (J.GT.NRL) GO TO 180
C                                       if a flagged run go get next
         IF (IXY(1,J).LT.0) GO TO 130
C                                       Else, unflagged run,
C                                       Set object number to dump
            INJ=ITEM(J)
C                                       If debugging print
            IF (DBUGIT) THEN
               WRITE(MSGTXT,2000) J, NRL, INJ, NITEM
               CALL MSGWRT(5)
               END IF
C                                      Add up the pieces
            AREA=0.
            XC=0.
            YC=0.
            ZS=0.
C                                     for all other runs
            DO 150 I=J,NRL
C                                     If apart of the item
               IF(ITEM(I).EQ.INJ) THEN
C                                     AREA:
                  IAI=IXY(2,I)
                  IBI=IXY(3,I)
                  DELI=(IBI-IAI+1)
                  AREA=AREA+DELI
C                                     ISOPHOTAL FLUX SUMMATION:
                  ZS=ZS+XS(1,I)
C                                     X,Y-CENTROID:
                  XC=XC+XS(2,I)
                  YC=YC+XS(3,I)
                  IF (DBUGIT) THEN
                     WRITE (MSGTXT,2100) I, IXY(1,I), IAI, IBI
                     CALL MSGWRT(5)
                     WRITE (MSGTXT,2200) I, XS(1,I), XS(2,I), XS(3,I)
                     CALL MSGWRT(5)
                     END IF
C                                      End if part of object
                  END IF
C                                      End of for all other runs loop
  150          CONTINUE
C                                      TEST AGAINST AREA THRESHOLD:
C                                      WRITE SUMMARY OF PEAK:
            P(1) = XC/ZS + IPOS(1)
            P(2) = YC/ZS
C                                      Set Star size, area+intensity
            P(3) = A1 * SQRT(ABS(AREA)) * CELLS(1)
            P(3) = P(3) + (A2*ZS) + A3
            P(4) = P(3)
C                                      if star covers enough area
            IF (AREA.GT.MNAREA .AND. AREA.LT.MXAREA) THEN
C                                      Write out a single ST entry
               IF (IRNO .GE. MXSTAR) THEN
                  MSGTXT = 'STLOC: Maximum number of stars exceeded'
                  CALL MSGWRT(8)
                  IERR = IRNO
                  GO TO 999
                  END IF
               IRNO = IRNO + 1
               STRCHR = 'S '
               P(5) = 0.
               P(6) = STTYPE
C                                       If converting to RA and DEC
               IF (DORADC) THEN
                  XC = P(1)
                  YC = P(2)
                  CALL XYVAL ( XC, YC, P(1), P(2), DUMMY, IERR)
C                                       if a bad translation
                  IF (IERR.NE.0) THEN
C                                       return the pixel numbers
                     P(1) = XC
                     P(2) = YC
                     END IF
                  END IF
C                                       Report progress
               IF ((((IRNO-1)/20)*20).EQ.IRNO-1.OR.IRNO.LT.5) THEN
                  WRITE (MSGTXT,1300)  IRNO, P(1), P(2), ZS
                  CALL MSGWRT(3)
                  END IF
               CALL STPUT (IRNO, 0, BUF, P(1), P(2), P(3), P(4),
     *            P(5), P(6), STRCHR, IERR)
C                                       End if enough area
               END IF
C                                       If normal Write, go get next
            IF (IERR.NE.0) THEN
C                                       Else, error write message
               MSGTXT = 'STFND: STPUT I/O ERROR, QUITTING'
               CALL MSGWRT(8)
               GO TO 999
               END IF
C                                      remove this item from runs
            K=J-1
C                                      for all other runs
            DO 170 I=J,NRL
C                                      if not current item
               IF (ITEM(I).NE.INJ.OR.IXY(1,I).LT.0) THEN
C                                      move over current item
                  K=K+1
C                                      compress list, copy parts
                  DO 165 L=1,3
                     IXY(L,K) = IXY(L,I)
                     XS(L,K)  = XS(L,I)
                     IF (K.NE.I) THEN
                        IXY(L,I) = 0
                        XS(L,I)  = 0
                        END IF
  165                CONTINUE
C                                      Transfer item number
                  ITEM(K) = ITEM(I)
C                                      If not same object, init object
                  IF (I.NE.K) ITEM(I) = 0
C                                      End if not part of current item
                  END IF
C
  170       CONTINUE
C                                      Now init parts of lists not used
            IF (K.LT.NRL) THEN
C                                      For all unused pieces
               DO 175 I = K+1, NRL
C                                      For each of three elements
                  DO 172 L=1,3
                     IXY(L,I) = 0
                     XS(L,I)  = 0
 172                 CONTINUE
                  ITEM(I) = 0
 175              CONTINUE
C                                      End if list must be initialed
               END IF
C                                      list compressed, current runs
            NRL=K
C                                      go find next object to write
            GO TO 140
C                                      If any runs left,
  180       IF (NRL.GT.0) THEN
C                                      CLEAR THE FLAGS:
               DO 190 I=1,NRL
                  IF (IXY(1,I).LT.0) IXY(1,I)=-IXY(1,I)
  190             CONTINUE
               END IF
C                                      End if previous rows
            END IF
C                                      Only initialize once
 999  ONCE = .TRUE.
      RETURN
C-----------------------------------------------------------------------
1000  FORMAT ('STLOC:  Searching',I5,' by',I5,' pixel box for up to',I6,
     *   ' stars')
1100  FORMAT ('STLOC:  Star Threshold Level is',1PE10.3)
1110  FORMAT ('STLOC:  Star Area between      ',1PE10.3,' and',
     *   E10.3,' Pixels')
1130  FORMAT ('STLOC:  Merging objects separated by  ',I6,' Pixels')
1200  FORMAT ('STLOC:  Currently finding stars on row',I6)
1300  FORMAT ('STLOC:  Star',I6,' at ',F11.4,',',F11.4,'; Sum =',
     *         1PE10.3)
2000  FORMAT ('DBUGI: Writing RUN',I4,' of',I4,'; ITEM',I4,' of',I4)
2100  FORMAT ('DBUGI: RUN',I4,' ROW',I10,'; COLS',I10,' to',I10)
2200  FORMAT ('DBUGI: RUN',I4,' SUM',1PE10.3,'; X,Y',E10.3,' to',E10.3)
      END
