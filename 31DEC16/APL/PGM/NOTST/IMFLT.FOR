LOCAL INCLUDE 'IMFLT.INC'
C                                       Local include for IMFLT
      INCLUDE 'INCS:PMAD.INC'
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD(256), CATBLK(256), JBUFSZ, ICODE, SCRTCH(512)
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      REAL      XSEQIN, XDISKI, XSEQO, XDISKO, BLC(7), TRC(7), XBOXES,
     *   BOX(4,10), APARM(10), PIXR(2), BUFF1(MABFSS), BUFF2(MABFSS)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSEQIN, XDISKI, XNAMOU, XCLAOU,
     *   XSEQO, XDISKO, BLC, TRC, XBOXES, BOX, APARM, PIXR
      COMMON /PARMS/ SEQIN, SEQOUT, DISKIN, DISKO, NEWCNO, OLDCNO,
     *   CATOLD, JBUFSZ, ICODE
      COMMON /BUFRS/ BUFF1, BUFF2, SCRTCH
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /MAPHDR/ CATBLK
C                                       End IMFLT
LOCAL END
      PROGRAM IMFLT
C-----------------------------------------------------------------------
C!  Fits a plane to selected portions of an images and flattens it.
C#  Map-util Modeling
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2008, 2010
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
C   IMFLT fits a plane to the data included in the boxes defined by
C   BOX.  The image is the flattened using this plane.
C   APARM indicates how the fit plane is used.
C
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input image.
C      INCLASS        CLAIN         Class of input image.
C      INSEQ          SEQIN         Seq. of input image.
C      INDISK         DISKIN        Disk number of input image.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      BLC(7)         BLC           Bottom left corner of subimage
C                                   of input image.
C      TRC(7)         TRC           Top right corner of subimage.
C      NBOXES         NBOXES        Number of boxes in fit.
C      BOX(4,10)      BOX           BLC and TRC for each box.
C      APARM(10)      APARM         (1) determines order of the fit.
C                                     =  0 only the mean value is used.
C                                     =  1 the fit plane is used.
C                                     = -1 then a plane whose slope
C                                          in direction APARM(2)
C                                          is removed from the data.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'IMFLT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'IMFLT '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FLTIN (PRGM, IRET)
C                                       Call routine that flattens
C                                       the image.
      IF (IRET.EQ.0) CALL FLATEN (IRET)
C                                       History
      IF (IRET.EQ.0) CALL FLTHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, SCRTCH)
C
 999  STOP
      END
      SUBROUTINE FLTIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   FLTIN gets input parameters for IMFLT and creates an output file.
C   Inputs:  PRGN    C*6       Program name
C   Output:  IRET    I         Error code: 0 => ok
C                                4 => user routine detected error.
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in IMFLT for more details.
C-----------------------------------------------------------------------
      CHARACTER  STAT*4, PRGN*6, MTYPE*2
      HOLLERITH CATH(256)
      INTEGER   IRET, IERR, NPARM, IROUND
      REAL      CATR(256)
      INCLUDE 'IMFLT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATR, CATH, CATBLK)
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Get input parameters.
      NPARM = 81
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, SCRTCH, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (IRET, SCRTCH, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQIN = IROUND (XSEQIN)
      SEQOUT = IROUND (XSEQO)
      DISKIN = IROUND (XDISKI)
      DISKO = IROUND (XDISKO)
C                                       Convert characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
C
      IF ((APARM(1).GE.0.) .OR. (APARM(2).NE.0.)) GO TO 20
          WRITE (MSGTXT,1060)
          GO TO 990
   20 CONTINUE
C                                       Create new file.
C                                       Get CATBLK from old file.
 30   OLDCNO = 1
      MTYPE = 'MA'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, MTYPE,
     *   NLUSER, STAT, SCRTCH, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
C                                       Read CATBLK and mark 'READ'.
 40   CALL CATIO ('READ', DISKIN, OLDCNO, CATOLD, 'READ', SCRTCH, IERR)
      IF (IERR.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IERR
         GO TO 990
 50   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Copy old CATBLK to new.
      CALL COPY (256, CATOLD, CATBLK)
      IF ((PIXR(1).GE.PIXR(2)) .OR. (PIXR(1).GE.CATR(KRDMX)) .OR.
     *   (PIXR(2).LE.CATR(KRDMN))) THEN
         PIXR(1) = CATR(KRDMN)
         PIXR(2) = CATR(KRDMX)
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Set defaults on BLC,TRC
      CALL WINDOW (CATOLD(KIDIM), CATOLD(KINAX), BLC, TRC, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get user modification to CATBLK
      IRET = 4
      CALL FLTHED (IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, SCRTCH, IERR)
      IF (IERR.EQ.0) GO TO 70
         WRITE (MSGTXT,1050) IERR
         GO TO 990
 70   NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy keywords partly
      CALL KEYPCP (DISKIN, OLDCNO, DISKO, NEWCNO, 0, ' ', IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLTIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I3,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('FLTIN: ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('FLTIN: ERRIR...YOU MUST SPECIFY A SLOPE FOR A ',
     *               'CONSTRAINED FIT ')
      END
      SUBROUTINE FLATEN (IRET)
C-----------------------------------------------------------------------
C   FLATEN is called by IMFLT to read an image, and determine the best
C   fit slope (one plane at a time).  Then, it uses the values of APARM
C   to decide on the background to subtract off the image.
C   Output: IRET   I    Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER IFILE*48
      INTEGER   IBOX(4,10), NBOX
      INTEGER   IRET, IROUND, LUNI, LUNO, NYI, NXI, WINI(4), WINI2(4),
     *   NXO, NYO, WINO(4), BOI, BOO, LIM2, LIM3, LIM4, LIM5, LIM6,
     *   LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7), CORN(7), BOTEMP,
     *   KOFF, LIMO, IBIND, OBIND, INDI, INDO, LIM1, I, J
      REAL      OUTMAX, OUTMIN, CATR(256), OLDR(256), TEMP
      DOUBLE PRECISION    OLDD(128), CATD(128)
      REAL   SUMN, SUMX, SUMY, SUMZ, SUMXX, SUMYY, SUMXY, SUMXZ, SUMYZ
      LOGICAL   T, F, BLNKD
      REAL   DENOM, SLOPEX, SLOPEY, ZMEAN, YMEAN, XMEAN, SPHI, CPHI,
     *   SLOPE, ANGLE, RADDEG, XC, YC, X, Y
      INCLUDE 'IMFLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATBLK, CATR, CATD),  (CATOLD, OLDR, OLDD)
      DATA LUNI, LUNO /16,17/
      DATA T, F /.TRUE.,.FALSE./
      DATA RADDEG /57.29577852/
C-----------------------------------------------------------------------
C                                       Open and init for read
      CALL ZPHFIL ('MA', DISKIN, OLDCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, T, IRET)
      IF (IRET.LE.0) GO TO 10
         WRITE (MSGTXT,1000) IRET
         GO TO 990
 10   CONTINUE
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, T, T, IRET)
      IF (IRET.EQ.0) GO TO 30
         WRITE (MSGTXT,1020) IRET
         GO TO 990
C                                       Setup for I/O for input map.
 30   NXI = CATOLD(KINAX)
      NYI = CATOLD(KINAX+1)
C                                      Set input window for first pass.
C                                      For the first pass, the entire
C                                      image is read.
      WINI(1) = 1
      WINI(2) = 1
      WINI(3) = NXI
      WINI(4) = NYI
C                                      Box off positions are from the
C                                      BLC OF THE INPUT WINDOW
      NBOX = INT (XBOXES)
      CALL COPY (4, WINI, IBOX)
      I3 = 0
      IF (NBOX.GT.0) THEN
         DO 35 I = 1,NBOX
            I2 = 0
            DO 32 J = 1,4
               WINI2(J) = BOX(J,I) + 0.1
               IF (WINI2(J).GT.0) I2 = I2 + 1
 32            CONTINUE
            IF (I2.EQ.4) THEN
               I3 = I3 + 1
               CALL COPY (4, WINI2, IBOX(1,I3))
               END IF
 35         CONTINUE
         END IF
      NBOX = MAX (1, I3)
C                                       Set up window for output
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
C                                       Set up input window
C                                       for second pass
      WINI2(1) = BLC(1)
      WINI2(2) = BLC(2)
      WINI2(3) = TRC(1)
      WINI2(4) = TRC(2)
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
      BLNKD = F
C                                       Setup for looping
      LIM1 = TRC(1) - BLC(1) + 1.01
      LIM2 = TRC(2) - BLC(2) + 1.01
      LIM3 = TRC(3) - BLC(3) + 1.01
      LIM4 = TRC(4) - BLC(4) + 1.01
      LIM5 = TRC(5) - BLC(5) + 1.01
      LIM6 = TRC(6) - BLC(6) + 1.01
      LIM7 = TRC(7) - BLC(7) + 1.01
      KOFF = 0
      CORN(7) = 1
      LIMO = CATBLK(KINAX) - 1
      YC = LIM2 / 2.0
      XC = LIM1 / 2.0
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = BLC(7) + I7 - 0.9
         CORN(7+KOFF) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = BLC(6) + I6 - 0.9
            CORN(6+KOFF) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = BLC(5) + I5 - 0.9
               CORN(5+KOFF) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = BLC(4) + I4 - 0.9
                  CORN(4+KOFF) = I4
                  DO 300 I3 = 1,LIM3
                     IPOS(3) = BLC(3) + I3 - 0.9
                     CORN(3+KOFF) = I3
C                                    Initialize sums
      SUMN  = 0.
      SUMX  = 0.
      SUMY  = 0.
      SUMZ  = 0.
      SUMXX = 0.
      SUMYY = 0.
      SUMXY = 0.
      SUMXZ = 0.
      SUMYZ = 0.
C
C                                       Init. input file, first pass.
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1099) IRET
            GO TO 990
            END IF
 100     BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI, BUFF1, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 990
            END IF
C                                           Loop over the y axis
         DO 190 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
            Y = I2 - YC
C                                          Read one row of input file,
C                                          first pass.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                           Loop over the x axis
          DO 180 I1 = 1,LIM1
C                                           Is this pixel in a box ?
              DO 150 I = 1,NBOX
                  IF ((I1.GE.IBOX(1,I)).AND.(I1.LE.IBOX(3,I))
     *                                  .AND.
     *                 (I2.GE.IBOX(2,I)).AND.(I2.LE.IBOX(4,I)))
     *                                GO TO 160
  150         CONTINUE
C                                             No.
              GO TO 180
C                                             Yes.
  160         TEMP = BUFF1(IBIND+I1-1)
C                                                Include it in the sums.
              IF ((TEMP.NE.FBLANK) .AND. (TEMP.GE.PIXR(1)) .AND.
     *           (TEMP.LE.PIXR(2))) THEN
                 X = I1 - XC
                 SUMN  = SUMN  + 1.
                 SUMX  = SUMX  + X
                 SUMY  = SUMY  + Y
                 SUMZ  = SUMZ  + TEMP
                 SUMXX = SUMXX + X * X
                 SUMYY = SUMYY + Y * Y
                 SUMXY = SUMXY + X * Y
                 SUMXZ = SUMXZ + X * TEMP
                 SUMYZ = SUMYZ + Y * TEMP
                 END IF
  180        CONTINUE
  190     CONTINUE
C                                                Calculate the fit.
          IF (SUMN.EQ.0.) THEN
             WRITE (MSGTXT,1300) ' NO DATA POINTS IN BOXES '
             IRET = 2
             GO TO 990
             END IF
          WRITE (MSGTXT,1310) ' NUMBER OF FIT POINTS =', SUMN
          CALL MSGWRT (8)
          SUMXX = SUMXX - SUMX*SUMX/SUMN
          SUMYY = SUMYY - SUMY*SUMY/SUMN
          SUMXY = SUMXY - SUMX*SUMY/SUMN
          SUMXZ = SUMXZ - SUMX*SUMZ/SUMN
          SUMYZ = SUMYZ - SUMY*SUMZ/SUMN
          DENOM = SUMXY * SUMXY - SUMXX * SUMYY
          IF (DENOM.EQ.0.) THEN
             WRITE (MSGTXT,1300) ' INFINITE SLOPE ! '
             IRET = 2
             GO TO 990
             END IF
          YMEAN = SUMY / SUMN
          XMEAN = SUMX / SUMN
          ZMEAN = SUMZ / SUMN
          SLOPEX = (SUMXY*SUMYZ - SUMXZ*SUMYY) / DENOM
          SLOPEY = (SUMXY*SUMXZ - SUMYZ*SUMXX) / DENOM
C                                       Use APARM to determine the
C                                       type of fit desired.
C                                       Set coefficients appropriately
          I = INT (MIN (1., MAX(-1., APARM(1))))
          IF (I.EQ.1) THEN
             ZMEAN  = ZMEAN - SLOPEY*YMEAN - SLOPEX*XMEAN
C                                       Subtract constrained slope.
C                                       APARM(2) contains the direction
C                                       of zero slope in output image.
          ELSE IF (I.EQ.-1) THEN
             SPHI = SIN (APARM(2)/RADDEG)
             CPHI = COS (APARM(2)/RADDEG)
             ZMEAN  = ZMEAN - SLOPEX * CPHI * XMEAN
     *          - SLOPEY * SPHI * YMEAN
             SLOPEX = CPHI * SLOPEX
             SLOPEY = SPHI * SLOPEY
C                                       just the mean
          ELSE
             SLOPEX = 0.
             SLOPEY = 0.
             END IF
C                                       Output information about the fit
          SLOPE = SQRT  (SLOPEX*SLOPEX + SLOPEY*SLOPEY)
          ANGLE = 0.
          IF (ABS(SLOPEY).GT.1.E-6*ABS(SLOPEX)) ANGLE =
     *       ATAN (SLOPEX / SLOPEY) * RADDEG
          WRITE (MSGTXT,1310) ' Offset  is  ',ZMEAN
          CALL MSGWRT (4)
          IF (I.NE.0) THEN
             WRITE (MSGTXT,1310) ' X slope is  ', SLOPEX
             CALL MSGWRT (4)
             WRITE (MSGTXT,1310) ' Y slope is  ', SLOPEY
             CALL MSGWRT (4)
             WRITE (MSGTXT,1310) ' Total slope ',SLOPE
             CALL MSGWRT (4)
             WRITE (MSGTXT,1320) ' Direction   ',ANGLE, '  degrees'
             CALL MSGWRT (4)
             END IF
C                                       send to HI
          APARM(6) = ZMEAN
          APARM(7) = SLOPEX
          APARM(8) = SLOPEY
          APARM(9) = SLOPE
          APARM(10) = ANGLE
C*******************************************************************
C                                       First pass finished
C                                       Init. files for second pass.
C                                       Input file.
C
         CALL COMOFF (CATOLD(KIDIM), CATOLD(KINAX), IPOS(3), BOTEMP,
     *      IRET)
         IF (IRET.EQ.0) GO TO 205
            WRITE (MSGTXT,1099) IRET
            GO TO 990
  205    BOI = BOTEMP + 1
         CALL MINIT ('READ', LUNI, INDI, NXI, NYI, WINI2, BUFF1, JBUFSZ,
     *      BOI, IRET)
         IF (IRET.EQ.0) GO TO 210
            WRITE (MSGTXT,1100) 'READ', IRET
            GO TO 990
  210    CONTINUE
C                                        Output file.
         CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), CORN(3), BOTEMP,
     *      IRET)
         BOO = BOTEMP + 1
         CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFF2, JBUFSZ,
     *      BOO, IRET)
         IF (IRET.EQ.0) GO TO 211
             WRITE (MSGTXT,1100) 'WRIT', IRET
             GO TO 990
  211    CONTINUE
C                                                    Loop
         DO 260 I2 = 1,LIM2
            IPOS(2) = BLC(2) + I2 - 0.9
            IPOS(1) = IROUND (BLC(1))
            Y = I2 - YC
            TEMP = ZMEAN + SLOPEY * Y
C
C                                       Read input file, second pass.
            CALL MDISK ('READ', LUNI, INDI, BUFF1, IBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'READ', IRET
               GO TO 990
               END IF
C                                       It is necessary to setup
C                                       output file here.
C                                       First call initializes pointers.
            CALL MDISK ('WRIT', LUNO, INDO, BUFF2, OBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1120) 'WRIT', IRET
               GO TO 990
                END IF
C                                       Copy to buffer.
             DO 250 I1 = 1,LIM1
                X = I1 - XC
C                                        Blanked data?
                IF (BUFF1(IBIND+I1-1).EQ.FBLANK) THEN
                   BUFF2(OBIND+I1-1) = FBLANK
                   BLNKD = .TRUE.
C                                        subtract off fitted background
C                                        move data to output buffer.
                ELSE
                   BUFF2(OBIND+I1-1) = BUFF1(IBIND+I1-1) - TEMP
     *                - SLOPEX * X
                   OUTMAX = MAX (OUTMAX, BUFF2(OBIND+I1-1))
                   OUTMIN = MIN (OUTMIN, BUFF2(OBIND+I1-1))
                   END IF
 250            CONTINUE
 260         CONTINUE
 300     CONTINUE
C                                         Flush buffer.
                 CALL MDISK ('FINI', LUNO, INDO, BUFF2, OBIND, IRET)
                 IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1120) 'FINI', IRET
                     GO TO 990
                     END IF
C                                       Update CATBLK.
                  CATR(KRDMX) = OUTMAX
                  CATR(KRDMN) = OUTMIN
                  CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST',
     *               SCRTCH, IRET)
                  IF (IRET.EQ.0) GO TO 400
                     WRITE (MSGTXT,1260) IRET
                     GO TO 990
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       Close input map.
      CALL ZCLOSE (LUNI, INDI, IRET)
 720  CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLATEN: ERROR',I3,' OPENING INPUT FILE')
 1020 FORMAT ('FLATEN: ERROR',I5,' OPENING OUTPUT FILE')
 1099 FORMAT ('FLATEN: COMOFF ERROR',I3)
 1100 FORMAT ('FLATEN: INIT-FOR-',A4,' ERROR',I3)
 1113 FORMAT ('FLATEN: OUTCNT.LT.0! Too many output rows returned.')
 1120 FORMAT ('FLATEN: ',A4,' ERROR',I3)
 1180 FORMAT ('FLATEN: DIDDLE ERROR',I3)
 1200 FORMAT ('FLATEN: INFINITE SLOPE IN FITTING....BAD DATA ')
 1260 FORMAT ('FLATEN: CATIO ERROR',I3,' UPDATING CATBLK')
 1300 FORMAT ('FLATEN: ',A)
 1310 FORMAT ('FLATEN: ',A,1PE15.8)
 1320 FORMAT ('FLATEN: ',A,F7.1,2X,A)
      END
      SUBROUTINE FLTHIS
C-----------------------------------------------------------------------
C   FLTHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER HILINE*72, NOTTYP*2
      INTEGER   LUN1, LUN2, IERR, I, IROUND
      LOGICAL   T
      INCLUDE 'IMFLT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'  '/
C-----------------------------------------------------------------------
C
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, OLDCNO, NEWCNO, CATBLK,
     *   SCRTCH(257), SCRTCH, IERR)
      IF (IERR.LE.2) GO TO 10
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 20
C                                       New history
 10   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, SCRTCH,
     *   IERR)
      IF (IERR.NE.0) GO TO 20
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2,
     *   SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       BLC
      WRITE (HILINE,2000) TSKNAM, BLC
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       TRC
      WRITE (HILINE,2001) TSKNAM, TRC
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C                                       pixrange
      WRITE (HILINE,2003) TSKNAM, PIXR
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR. NE. 0) GO TO 20
C                                        APARM
      I = IROUND (APARM(1))
      I = MAX (-1, MIN (1, I))
      WRITE (HILINE,2004) TSKNAM, I
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR.NE.0) GO TO 20
C
      IF (I.EQ.-1) THEN
         WRITE (HILINE,2005) TSKNAM, APARM(2)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR. NE. 0) GO TO 20
         END IF
C
      WRITE (HILINE,2006) TSKNAM, APARM(6)
      CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
      IF (IERR. NE. 0) GO TO 20
C
      IF (I.NE.0) THEN
         WRITE (HILINE,2007) TSKNAM, APARM(7)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR. NE. 0) GO TO 20
         WRITE (HILINE,2008) TSKNAM, APARM(8)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR. NE. 0) GO TO 20
         WRITE (HILINE,2009) TSKNAM, APARM(9)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR. NE. 0) GO TO 20
         WRITE (HILINE,2010) TSKNAM, APARM(10)
         CALL HIADD (LUN2, HILINE, SCRTCH, IERR)
         IF (IERR. NE. 0) GO TO 20
         END IF
C                                       Close HI file
 20   CALL HICLOS (LUN2, T, SCRTCH, IERR)
C                                        Copy tables
      CALL ALLTAB (1, NOTTYP, LUN1, LUN2, DISKIN, DISKO, OLDCNO,
     *   NEWCNO, CATBLK, SCRTCH(257), SCRTCH, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, 'REST', SCRTCH, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FLTHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 2000 FORMAT (A6,'BLC =',6(F6.0,','),F6.0)
 2001 FORMAT (A6,'TRC =',6(F6.0,','),F6.0)
 2003 FORMAT (A6,'PIXRANGE=',2(1PE12.4))
 2004 FORMAT (A6,'APARM(1) = ',I2)
 2005 FORMAT (A6,'APARM(2) = ',F8.3)
 2006 FORMAT (A6,'/ MEAN PIX  = ',1PE15.8)
 2007 FORMAT (A6,'/ SLOPE X   = ',1PE15.8)
 2008 FORMAT (A6,'/ SLOPE Y   = ',1PE15.8)
 2009 FORMAT (A6,'/ SLOPE TOT = ',1PE15.8)
 2010 FORMAT (A6,'/ DIRECTION = ',F10.5)
      END
      SUBROUTINE FLTHED (IRET)
C-----------------------------------------------------------------------
C   FLTHED is a routine in which sets the limits on the axes of the new
C   catalog.  It can be safely replaced by the template routine, NEWHED,
C   associated with task TAFFY.
C   See initial comments in that subroutine for more information.
C-----------------------------------------------------------------------
      CHARACTER FCHARS(3)*4, CHTMP*4
      HOLLERITH CATH(256), OLDH(256)
      INTEGER   LIMIT, I, FIRSTI, FIRSTO, IRET
      REAL      CATR(256), OLDR(256)
      DOUBLE PRECISION    CATD(128), OLDD(128)
      INTEGER   INDXI, INDEX
      INCLUDE 'IMFLT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      EQUIVALENCE  (CATBLK, CATR, CATD, CATH)
      EQUIVALENCE  (CATOLD, OLDR, OLDD, OLDH)
      DATA FCHARS /'FREQ','VELO','FELO'/
C-----------------------------------------------------------------------
C                                       Set axes in output CATBLK.
      FIRSTI = -1
      FIRSTO = -1
      LIMIT = CATOLD(KIDIM)
C                                       Copy/update axis values
      DO 80 I = 1,LIMIT
         CATBLK(KINAX+FIRSTO+I) = TRC(I+FIRSTI+1) -
     *      BLC(I+FIRSTI+1) + 1.01
         CATR(KRCRP+FIRSTO+I) = OLDR(KRCRP+FIRSTI+I) - BLC(I+FIRSTI+1)
     *      + 1.0
         CATR(KRCIC+FIRSTO+I) = CATR(KRCIC+FIRSTI+I)
         CATD(KDCRV+FIRSTO+I) = OLDD(KDCRV+FIRSTI+I)
         INDXI = KHCTP + (I+FIRSTI) * 2
         INDEX = KHCTP + (I-1) * 2
         CATH(INDEX) = OLDH(INDXI)
         CATH(INDEX+1) = OLDH(INDXI+1)
         IF (CATBLK(KIALT).EQ.0) GO TO 80
            CALL H2CHR (4, 1, CATH(INDEX), CHTMP)
            IF ((CHTMP.EQ.FCHARS(1)) .OR. (CHTMP.EQ.FCHARS(2)) .OR.
     *         (CHTMP.EQ.FCHARS(3))) CATR(KRARP) = CATR(KRARP) -
     *         BLC(FIRSTI+I+1) + 1.0
 80      CONTINUE
C                                       Finished.
      IRET = 0
      GO TO 999
C                                       Error.
 990  CALL MSGWRT (8)
C
 999  RETURN
      END
