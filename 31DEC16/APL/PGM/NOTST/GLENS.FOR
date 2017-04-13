LOCAL INCLUDE 'GLENS.INC'
C                                       Local include for LENS
      INTEGER   MAXMAP
      PARAMETER (MAXMAP=512)
      INCLUDE 'INCS:PMAD.INC'
C
      INTEGER   SEQOUT, DISKO, NEWCNO, NUMHIS, JBUFSZ, ICODE, LUNIN,
     *   FINDIN
      CHARACTER HISCRD(10)*64, SOURCE*8, NAMOUT*12, CLAOUT*6
      HOLLERITH XSOURC(2), XNAMOU(3), XCLAOU(2)
      REAL      XMSIZE(2), XSEQO, XDISKO, APARM(10), BPARM(10),
     *          CPARM(10), DPARM(10), BUFFER(MABFSS)
C                                       Program commons
      COMMON /INPARM/ XSOURC, XMSIZE,
     *   XNAMOU, XCLAOU, XSEQO, XDISKO, APARM, BPARM, CPARM, DPARM
      COMMON /PARMS/ SEQOUT, DISKO, NEWCNO,
     *   JBUFSZ, ICODE, LUNIN, FINDIN, NUMHIS
      COMMON /BUFRS/ BUFFER
      COMMON /CHRCOM/ HISCRD, SOURCE, NAMOUT, CLAOUT
LOCAL END
LOCAL INCLUDE 'EPSLN.INC'
C                                       Local include of small values
      REAL      EPSLN, EPSLN2, MAXMAG
      PARAMETER (EPSLN=1.0E-3, EPSLN2=EPSLN*EPSLN, MAXMAG=0.01)
LOCAL END
      PROGRAM GLENS
C-----------------------------------------------------------------------
C! GLENS Simulates a gravitational lens acting on a 3 component source
C# Map-Experimental
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2000, 2015
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
C   Gravitational LENS modeling program
C   GLENS Creats a jacobian or source plane function for an elliptical
C   galaxy lens model with paramters b, epsilon, theta
C   and core radius S.
C   A circular source model can be created with CPARM(7,8 and 9)
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      SOURCE         SOURCE        Object name.
C      IMSIZE         XMSIZE        Image size in pixels.
C      OUTNAME        NAMOUT        Name of the output image
C                                   Default output is input image.
C      OUTCLASS       CLAOUT        Class of the output image.
C                                   Default is input class.
C      OUTSEQ         SEQOUT        Seq. number of output image.
C      OUTDISK        DISKO         Disk number of the output image.
C      APARM(10)      APARM         Source Model (First Component)
C                                   1:XS1      2:YS1
C                                   3:MAJAX1;  4;MINAX1
C                                   5:TH1 (Degrees);  6:INTENS1A
C                                   7:INTENS1B
C      BPARM(10)      BPARM         Source Model (Second Component)
C                                   1:XS2      2:YS2
C                                   3:MAJAX2;  4;MINAX2
C                                   5:TH2;  6:INTENS2A
C                                   7:INTENS2B
C      CPARM(10)      CPARM         Source Model (Third Component)
C                                   1:XS3      2:YS3
C                                   3:MAJAX3;  4;MINAX3
C                                   5:TH3;  6:INTENS3A
C                                   7:INTENS3B
C      DPARM(10)      DPARM         Lens Model
C                                   1:X0;      2:Y0
C                                   3:Theta;   4:B
C                                   5:Epsilon; 6:S
C                                   7:Alpha
C                                   8:Model Type 0=1=> Blandford
C                                     2=> Point Mass
C                                     3=> Narayan Galaxy
C                                     4=> King Model
C                                     5=> de Vaucouleurs
C                                   9:Include Source model >0 yes
C                                     Magnifcation in image plane < 0
C                                   10:>0 Source Plane, else Image Plane
C   Programmer Glen Langston, June 1989
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'GLENS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA PRGM /'GLENS '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL CANIN (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL SENDMA (IRET)
C                                       History
      IF (IRET.EQ.0) CALL CANHIS
C                                       Close down files, etc.
      CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE CANIN (PRGN, IRET)
C-----------------------------------------------------------------------
C   CANIN gets input parameters for LENS and creates an output file.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      IRET    I    Error code: 0 => ok
C                     4 => user routine detected error.
C                     5 => catalog troubles
C                     8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in LENS for more details.
C
C   To change the adverb list sent to this task change:
C   1)  the inputs file.
C   2)  the contents of COMMON /INPARM/.  Remember all adverbs are sent
C       as R,    OUTNAME etc. are 12 char. 3 words;
C       OUTCLASS etc. are 6 char., 2 words.
C       Values will be filled into COMMON /INPARM/ in the order
C       specified in the inputs file.
C   3)  If the first adverb is not FILEIN then replace
C       XFILEI in the call to GTPARM with the name of the first
C       adverb.
C   4)  Change the value of NPARM sent to GTPARM to the number of
C       R   words desired.
C-----------------------------------------------------------------------
      INTEGER   IRET
      CHARACTER PRGN*6
C
      CHARACTER BLANK*6, DEFNAM*12, OLDNAM*12, LENSTY(7)*12
      INTEGER   IERR, NPARM, IROUND, NX, NY
      LOGICAL   T, F
      REAL      CELLS(2)
      DOUBLE PRECISION REFVAL(2)
      INCLUDE 'GLENS.INC'
      INCLUDE 'EPSLN.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA BLANK /'      '/
      DATA T, F /.TRUE.,.FALSE./
      DATA DEFNAM /'GRAVITY LENS'/
      DATA LENSTY /'IMAGE  PLANE','SOURCE PLANE','BLANDFORD   ',
     *             'POINT MASS  ','NARAYAN     ','KING GALAXY ',
     *             'DEVAUCOULEUR'/
      DATA CELLS/2.777777E-4,2.777777E-4/, REFVAL/0.0,0.0/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = 2 * MABFSS
      IRET = 0
      NUMHIS = 0
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
C                                       Init CATBLK.
      CALL FILL (256, 0, CATBLK)
C                                       Get input parameters.
      NPARM = 51
      CALL GTPARM (PRGN, NPARM, RQUICK, XSOURC, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         IRET = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (IRET, BUFFER, IERR)
      IF (IRET.NE.0) GO TO 999
      IRET = 5
C                                       Crunch input parameters.
      SEQOUT = IROUND (XSEQO)
      DISKO = IROUND (XDISKO)
C                                       Characters
      CALL H2CHR (8,  1, XSOURC, SOURCE)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6,  1, XCLAOU, CLAOUT)
      IF (SOURCE.EQ.'        ') SOURCE = 'Einstein'
C                                       Create new file.
C                                       def. name is lens model
      OLDNAM = LENSTY(3)
      IF (DPARM(8).EQ.3) OLDNAM = LENSTY(5)
      IF (DPARM(8).EQ.4) OLDNAM = LENSTY(6)
      IF (DPARM(8).EQ.5) OLDNAM = LENSTY(7)
C                                       Allow only point or blandford
      IF (DPARM(8).GE.2) THEN
C                                       Point lens
         DPARM(8) = 2
         OLDNAM = LENSTY(4)
      ELSE
C                                       Else Blandford lens
         DPARM(8) = 1
         OLDNAM = LENSTY(3)
         END IF
C                                       If Source or Image plane
      IF (DPARM(10).GT.EPSLN) OLDNAM = LENSTY(2)
C                                       DPARM(10)<=0 => IMAGE plane
      IF (DPARM(10).LT.-EPSLN) OLDNAM = LENSTY(1)

      CALL MAKOUT (OLDNAM, BLANK, 0, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'MA', KHPTYO, CATH(KHPTY))
      CATBLK(KIIMS) = SEQOUT
C                                       init map size
      IF (XMSIZE(1) .LE. 0) XMSIZE(1) = MAXMAP/4
      IF (XMSIZE(2) .LE. 0) XMSIZE(2) = MAXMAP/4
C                                       Make map size even
      NX = (IROUND( XMSIZE(1))+1)/2
      NY = (IROUND( XMSIZE(2))+1)/2
C                                       Limit to valid values
      XMSIZE(1) = MIN( MAX ( NX*2, 1), MAXMAP)
      XMSIZE(2) = MIN( MAX ( NY*2, 1), MAXMAP)
C                                       If making both image and source
C                                       Make twice as big
      IF (DPARM(10).EQ.0) XMSIZE(1) = 2 * XMSIZE(1)
C                                       Get user modification to CATBLK
      IRET = 4
C                                       Set map header defaults
      CALL NEWHED ('RA---SIN','DEC--SIN','Magnify ', SOURCE,
     *   REFVAL, XMSIZE, CELLS, IRET)
      IF (IRET.NE.0) GO TO 999
C                                       Create output file.
      NEWCNO = 1
      IRET = 4
      CALL MCREAT (DISKO, NEWCNO, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = NEWCNO
      FRW(NCFILE) = 2
      IRET = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Set obs. date=current date.
      CATH(KHDOB) = CATH(KHDMP)
      CATH(KHDOB+1) = CATH(KHDMP+1)
      GO TO 999

 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1060 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE SENDMA (IRET)
C-----------------------------------------------------------------------
C   SENDMA accepts an image one row at a time from the user supplied
C   routine.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INTEGER   LUNO, BOTEMP, NXO, NYO, WINO(4), BOO, LIM2, LIM3, LIM4,
     *   LIM5, LIM6, LIM7, I1, I2, I3, I4, I5, I6, I7, IPOS(7), LIMO,
     *   LIMIT, OBIND, LUN1, LUN2, INDO, LIM1
      REAL      OUTMAX, OUTMIN
      CHARACTER IFILE*48, REST*4
      LOGICAL   T, F, BLNKD
      INCLUDE 'GLENS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA LUNO, LUN1, LUN2 /16,17,18/, OBIND/1/
      DATA REST /'REST'/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Create scratch file.
C                                       Open vis file for write
      CALL ZPHFIL ('MA', DISKO, NEWCNO, 1, IFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
C                                       Setup for I/O
      NXO = CATBLK(KINAX)
      NYO = CATBLK(KINAX+1)
      WINO(1) = 1
      WINO(2) = 1
      WINO(3) = NXO
      WINO(4) = NYO
      OUTMAX = -1.0E20
      OUTMIN = 1.0E20
      BLNKD = F
C                                       Setup for looping
      LIM1 = MAX (1, CATBLK(KINAX))
      LIM2 = MAX (1, CATBLK(KINAX+1))
      LIM3 = MAX (1, CATBLK(KINAX+2))
      LIM4 = MAX (1, CATBLK(KINAX+3))
      LIM5 = MAX (1, CATBLK(KINAX+4))
      LIM6 = MAX (1, CATBLK(KINAX+5))
      LIM7 = MAX (1, CATBLK(KINAX+6))
      IPOS(1) = 1
      LIMO = CATBLK(KINAX) - 1
C                                       Loop
      DO 700 I7 = 1,LIM7
         IPOS(7) = I7
         DO 600 I6 = 1,LIM6
            IPOS(6) = I6
            DO 500 I5 = 1,LIM5
               IPOS(5) = I5
               DO 400 I4 = 1,LIM4
                  IPOS(4) = I4
                  DO 300 I3 = 1,LIM3
      IPOS(3) = I3
C                                       Init output file.
      CALL COMOFF (CATBLK(KIDIM), CATBLK(KINAX), IPOS(3), BOTEMP, IRET)
      BOO = BOTEMP + 1
      CALL MINIT ('WRIT', LUNO, INDO, NXO, NYO, WINO, BUFFER, JBUFSZ,
     *   BOO, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1100) 'WRIT', IRET
         GO TO 990
         END IF
      DO 250 I2 = 1,LIM2
         IPOS(2) = I2
C                                       Write.
         CALL MDISK ('WRIT', LUNO, INDO, BUFFER, OBIND, IRET)
         OBIND = OBIND - 1
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1120) 'WRIT', IRET
            GO TO 990
            END IF
C                                       Call MAKMAP
         OBIND = OBIND + 1
         CALL MAKMAP (IPOS, BUFFER(OBIND), IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1180) IRET
            GO TO 990
            END IF
C                                       Check max, min, blanking.
         LIMIT = OBIND + LIMO
         DO 200 I1 = OBIND,LIMIT
            BLNKD = BLNKD .OR. (BUFFER(I1).EQ.FBLANK)
            IF (BUFFER(I1).EQ.FBLANK) GO TO 200
               OUTMAX = MAX (OUTMAX, BUFFER(I1))
               OUTMIN = MIN (OUTMIN, BUFFER(I1))
 200           CONTINUE
 250     CONTINUE
C                                       Dump plane to output.
C                                       Flush buffer.
      CALL MDISK ('FINI', LUNO, INDO, BUFFER, OBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1120) 'FINI', IRET
         GO TO 990
         END IF
C                                       Update CATBLK.
      CATR(KRDMX) = OUTMAX
      CATR(KRDMN) = OUTMIN
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, REST, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1260) IRET
         GO TO 990
         END IF
 300  CONTINUE
 400              CONTINUE
 500           CONTINUE
 600        CONTINUE
 700     CONTINUE
C                                       Mark blanking in CATBLK.
      CATR(KRBLK) = 0.0
      IF (BLNKD) CATR(KRBLK) = FBLANK
C                                       Final call to MAKMAP
      IPOS(1) = -1
      CALL MAKMAP (IPOS, BUFFER, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1180) IRET
         GO TO 990
         END IF
C                                       Close files
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('SENDMA: ERROR',I3,' OPENING SCRATCH FILE')
 1100 FORMAT ('SENDMA: INIT-FOR-',A4,' ERROR',I3)
 1120 FORMAT ('SENDMA: ',A4,' ERROR',I3)
 1180 FORMAT ('SENDMA: MAKMAP ERROR',I3)
 1260 FORMAT ('SENDMA: CATIO ERROR',I3,' UPDATING CATBLK')
      END
      SUBROUTINE CANHIS
C-----------------------------------------------------------------------
C   CANHIS copies and updates history file.
C-----------------------------------------------------------------------
      CHARACTER ATIME*8, ADATE*12, HILINE*72, LABEL*8, REST*4
      INTEGER   LUN, IERR, I, TIME(3), DATE(3)
      LOGICAL   T, F
      INCLUDE 'GLENS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA LUN /27/, REST /'REST'/, T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Create/open hist. file.
      CALL HICREA (LUN, DISKO, NEWCNO, CATBLK, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 999
         END IF
C                                       Get current date/time.
      CALL ZDATE (DATE)
      CALL ZTIME (TIME)
      CALL TIMDAT (TIME, DATE, ATIME, ADATE)
C                                       Write first record.
      WRITE (HILINE,1010) TSKNAM, NLUSER, ADATE, ATIME
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       New history
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN,
     *   BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       SOURCE
      WRITE (HILINE,2000) TSKNAM, SOURCE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       IMSIZE
      WRITE (HILINE,2001) TSKNAM, XMSIZE
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Lens Model parameters
      WRITE (HILINE,2002) TSKNAM, DPARM(1), DPARM(2), DPARM(3)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Area
      WRITE (HILINE,2003) TSKNAM, DPARM(4), DPARM(6),DPARM(5)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Lens Model
      HILINE = 'GLENS  Blandford LENS '
      IF (DPARM(8) .GE. 2) HILINE = 'GLENS  Point LENS '
C     IF (DPARM(8) .EQ. 3) HILINE = 'GLENS  Narayan LENS '
C     IF (DPARM(8) .EQ. 4) HILINE = 'GLENS  King LENS '
C     IF (DPARM(8) .EQ. 5) HILINE = 'GLENS  De Vaucouleurs LENS '
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Source
      WRITE (HILINE,2013) TSKNAM, APARM(1), APARM(2), APARM(6)
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       Source
      WRITE (HILINE,2014) TSKNAM, APARM(3), APARM(4), APARM(5)-90.
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                       If second component
      IF (BPARM(3) .GT. 0) THEN
         WRITE (HILINE,2013) TSKNAM, BPARM(1), BPARM(2), BPARM(6)
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Source
         WRITE (HILINE,2014) TSKNAM, BPARM(3), BPARM(4), BPARM(5)-90.
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       If third component
      IF (CPARM(3) .GT. 0) THEN
         WRITE (HILINE,2013) TSKNAM, CPARM(1), CPARM(2), CPARM(6)
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 200
C                                       Source
         WRITE (HILINE,2014) TSKNAM, CPARM(3), CPARM(4), CPARM(5)-90.
         CALL HIADD (LUN, HILINE, BUFFER, IERR)
         IF (IERR.NE.0) GO TO 200
         END IF
C                                       AIPS release
      WRITE (HILINE,2004) TSKNAM, RLSNAM
      CALL HIADD (LUN, HILINE, BUFFER, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any user supplied history.
      IF (NUMHIS.GT.0) THEN
         WRITE (LABEL,1011) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN, HILINE, BUFFER, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
         END IF
C                                       Close HI file
 200  CALL HICLOS (LUN, T, BUFFER, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, NEWCNO, CATBLK, REST, BUFFER,
     *   IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CANHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ Image created by user',I5,' at ',A12,2X,A8)
 1011 FORMAT (A6,'/')
 2000 FORMAT (A6,' Source = ''',A8,'''')
 2001 FORMAT (A6,' Imsize = ',2F8.0)
 2002 FORMAT (A6,' X0, Y0 = ( ',F10.5,',',F10.5,'); Theta  = ',F10.5)
 2003 FORMAT (A6,' B0, S0 = ( ',F10.5,',',F10.5,'); Eccen. = ',F10.5)
 2004 FORMAT (A6,' RELEASE = ''',A7,' ''')
 2013 FORMAT (A6,' XS, YS = ( ',F10.5,',',F10.5,'); Intens = ',F10.5)
 2014 FORMAT (A6,' MAJ,MIN= ( ',F10.5,',',F10.5,'); Theta  = ',F10.5)
      END
      SUBROUTINE NEWHED (AXIS1, AXIS2, UNITS, SOURCE, REFVAL, XMSIZE,
     *   CELLS, IRET)
C-----------------------------------------------------------------------
C   NEWHED is a routine in which the user performs several operations
C   associated with beginning the task.  For many purposes simply
C   changing some of the values in the DATA statments will be all that
C   is necessary.  The following functions are/can be preformed
C   in NEWHED:
C       1) Creating the catalog header block to represent the
C   output file.  The MINIMUM information required here is that
C   required to define the size of the output file; ie.
C      CATBLK(KIDIM)= the number of axes,
C      CATBLK(KINAX+i) = the dimension of each axis, and
C      CATBLK(KIBPX) => 2 = real*4 pixel values.
C   Other changes can be made either here or in MAKMAP; the
C   catalog block will be updated when the history file is
C   written.
C    Input:
C     AXIS1     C*8     Units of first axis
C     AXIS2     C*8     Units of second axis
C     UNITS     C*8     Character string describing the map untis
C     SOURCE    C*8     Eight character Source Name
C     REFVAL    D*2     Reference Value in Degrees
C     XMSIZE    R*2     Map size in pixels
C     CELLS     R*2     Pixel size in Degrees
C    Input via Common:
C     CATBLK    I(256)  Output catalog header, also CATR, CATD
C                       The OUTNAME, OUTCLASS, OUTSEQ are entered
C                       elsewhere.
C    Output:
C     CATBLK    I(256)  Modified output catalog header.
C     IRET      I       Return error code, 0=>OK, otherwise abort.
C-----------------------------------------------------------------------
      CHARACTER AXIS1*8, AXIS2*8, UNITS*8, SOURCE*8
      INTEGER   IRET
      REAL XMSIZE(2), CELLS(2)
      DOUBLE PRECISION REFVAL(2)
C
      CHARACTER BLANK*8, ATYPES(7)*8
      INTEGER   I, NAXIS, IROUND, NCODE, INDEX
C      INCLUDE 'GLENS.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA  BLANK /'        '/
C                                       User definable values
      DATA NCODE /0/
C                                       NUMBER OF AXES AND TYPES.
C                                       (SET FOR TWO AXES = RA, DEC.)
      DATA NAXIS  /2/
      DATA ATYPES /'RA---SIN', 'DEC--SIN',
     *   'STOKES  ', 'FREQ    ', 3*'       '/
C-----------------------------------------------------------------------
C                                       set Axis Values
      ATYPES(1) = AXIS1
      ATYPES(2) = AXIS2
C                                       SET OUTPUT UNITS.
      CALL CHR2H (8, UNITS, 1, CATH(KHBUN))
C                                       FILL AXIS ARRAYS.
      DO 30 I = 1,KICTPN
C                                       INIT DIMENSION
         CATBLK(KINAX+I-1) = 1
C                                       INIT. INCREMENT.
         CATR(KRCIC+I-1) = 0.0
C                                       INIT. REF PIXEL.
         CATR(KRCRP+I-1) = 1.0
C                                       INIT. REF VALUE.
         CATD(KDCRV+I-1) = 0.0
C                                       FILL AXIS TYPE FROM
C                                       ATYPES OR BLANK.
         INDEX = KHCTP + (I-1) * 2
         CALL CHR2H (8, BLANK, 1, CATH(INDEX))
         IF (I.LE.NAXIS)
     *      CALL CHR2H (8, ATYPES(I), 1, CATH(INDEX))
C                                       blank random axies.
         INDEX = KHPTP + (I-1) * 2
         CALL CHR2H (8, BLANK, 1, CATH(INDEX))
 30      CONTINUE
C                                       init. ref value.
      CATD(KDCRV)   = REFVAL(1)
      CATD(KDCRV+1) = REFVAL(2)
C                                       init map size
      CATBLK(KINAX)   = MAX( IROUND( XMSIZE(1)), 1)
      CATBLK(KINAX+1) = MAX( IROUND( XMSIZE(2)), 1)
C                                       reinit ref pixel
      CATR(KRCRP)   = CATBLK(KINAX)  /2.
      CATR(KRCRP+1) = CATBLK(KINAX+1)/2.
C                                       Assume CELLSIZE in degrees.
C                                       NOTE: Ra decreases with
C                                       grid number.
      CATR(KRCIC) = - CELLS(1)
      CATR(KRCIC+1) = CELLS(2)
C                                       Fill other character strings.
C                                       Object.
      CALL CHR2H (8, SOURCE, 1, CATH(KHOBJ))
C                                       Observation date.
      CALL CHR2H (8, BLANK, 1, CATH(KHDOB))
C                                       Telescope.
      CALL CHR2H (8, BLANK, 1, CATH(KHTEL))
C                                       Receiver
      CALL CHR2H (8, BLANK, 1, CATH(KHINS))
C                                       Observer's name.
      CALL CHR2H (8, BLANK, 1, CATH(KHOBS))
C                                       Set number of axes.
      CATBLK(KIDIM) = NAXIS
      CATBLK(KIPCN) = 0
C                                       User ID
      CATBLK(KIIMU) = NLUSER
C                                       Miscellaneous items.
C                                       Epoch.
      CATR(KREPO) = 0.0
C                                       Convolving beam
      CATR(KRBMJ) = 0.0
      CATR(KRBMN) = 0.0
      CATR(KRBPA) = 0.0
C                                       Max. min.
      CATR(KRDMX) = 0.0
      CATR(KRDMN) = 0.0
C                                       Put other checks here.
C                                       Finished.
      IRET = 0
C
 999  RETURN
      END
      SUBROUTINE MAKMAP (IPOS, RESULT, IRET)
C-----------------------------------------------------------------------
C  Returns Lens Model image.
C  Inputs:
C   IPOS(7)   I    BLC (input image) of first value in DATA
C  Values from commons:
C   ICODE     I    Opcode number from list in NEWHED.
C   FBLANK    R    Value of blanked pixel.
C   CPARM(10) R    Input adverb array.
C   DPARM(10) R    Input adverb array.
C   CATBLK    I    Output catalog header (also CATR, CATD)
C  Output:
C   RESULT(*) R    Output row.
C   IRET      I    Return code   0 => OK
C                               >0 => error, terminate.
C
C  Output in COMMON
C  NUMHIS     I    # history entries (max. 10)
C  HISCRD(16,NUMHIS) R   History records
C  CATBLK     I    Catalog header block
C
C-----------------------------------------------------------------------
      INTEGER   IPOS(7), IRET
      REAL      RESULT(*)
C
      INCLUDE 'GLENS.INC'
      REAL      IMAGES(MAXMAP,MAXMAP),QUASAR(MAXMAP,MAXMAP)
      INCLUDE 'EPSLN.INC'
      INTEGER   BLAND, POINT, NARAYA, KING, DEVAUC
      PARAMETER (BLAND=1, POINT=2, NARAYA=3, KING=4, DEVAUC=5)
      INTEGER   NX, NY, I, J, SRCPLN, LTYPE, NXO2, NYO2,
     *   CNTMSG, IXS, IYS, X, Y
      REAL   TH0, X0, Y0, S0, EP0, B0, B2, XI, YI, XS, YS,
     *   S2, MAGI, MAGS, DR2, COSTH, SINTH, XPRM, YPRM, DX, DY
      REAL   X2SRC, Y2SRC, MAJB, MINB, RMAJB, RMINB, INTSB, INTSB2,
     *       X3SRC, Y3SRC, MAJC, MINC, RMAJC, RMINC, INTSC, INTSC2,
     *       X1SRC, Y1SRC, MAJA, MINA, RMAJA, RMINA, INTSA, INTSA2,
     *       COS1TH, SIN1TH, COS2TH, SIN2TH, COS3TH, SIN3TH
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      SAVE IMAGES, QUASAR
      DATA CNTMSG/0/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Check if last call
      IF (IPOS(1).LT.0) GO TO 999
C                                       fill array on first execution
      IF (IPOS(2) .LE. 1) THEN
         SRCPLN   = DPARM(10)
C                                       Get image size
         NX = CATBLK(KINAX)
         NY = CATBLK(KINAX+1)
C                                       If making both Source and Image
         IF (SRCPLN.EQ.0) NX = NX / 2.
C                                       IF zero, make an intersting lens
         IF (DPARM(1).EQ.0 .AND. DPARM(2).EQ.0 .AND.
     *       DPARM(4).EQ.0 .AND. DPARM(5).EQ.0 ) THEN
C                                       Lens in middle
            DPARM(1) = NX/2
            DPARM(2) = NY/2
C                                       orientation=30 deg. and radius
            DPARM(3) = 30.
            DPARM(4) = MAX(10., NX/5.)
C                                       make it slightly eccentric
            DPARM(5) = .1
C                                       add source
            DPARM(9) = 1
            END IF
C                                       get lens coordinate
         X0       = DPARM(1)
         Y0       = DPARM(2)
C                                       lens must not be on edge
         IF (X0.LT.1 .OR. X0 .GE. NX) X0 = NX/2
         IF (Y0.LT.1 .OR. Y0 .GE. NY) Y0 = NY/2
         DPARM(1) = X0
         DPARM(2) = Y0
C                                       Rotation
         TH0      = DPARM(3)
         COSTH    = COS(TH0*0.01745)
         SINTH    = SIN(TH0*0.01745)
C                                       Einstein ring size (pixels)
         B0       = DPARM(4)
         B2       = B0*B0
         EP0      = DPARM(5)
         S0       = DPARM(6)
C                                       lens type
         LTYPE    = DPARM(8)
C                                       Limit lens models
         LTYPE    = MAX ( BLAND, MIN(LTYPE, POINT))
C                                       core radius nonzero for devauc
         IF (LTYPE.EQ.DEVAUC .AND. S0 .LE. 0.) S0 = 1.
         S2       = S0*S0
C                                       read in source location
         X1SRC    = APARM(1)
         Y1SRC    = APARM(2)
C                                       if out of range put in center
         IF (X1SRC.LT.1 .OR. X1SRC .GE. NX) X1SRC = NX/2
         IF (Y1SRC.LT.1 .OR. Y1SRC .GE. NY) Y1SRC = NY/2
         APARM(1) = X1SRC
         APARM(2) = Y1SRC
C                                       major and minor axies
         MAJA = ABS(APARM(3))
         MINA = ABS(APARM(4))
C                                       Always have an A component
         IF (MAJA.LE.0) THEN
C                                       Make an interesting default
            MAJA = MAX(3,NX/40)
            Y1SRC = Y1SRC + (2.*MAJA)
            APARM(6) = 10
            END IF
C                                       If an A component
         IF (MINA .LE. 0.  .OR. MINA .GT. NX/2) MINA = MAJA
         IF (MAJA .GT. 0. .AND. MAJA .LT. NX/2) THEN
C                                       calc useful axis parameters
            RMAJA = 1./(MAJA*MAJA)
            RMINA = 1./(MINA*MINA)
C                                       first component orientation
C                                       orient to Clockwise from N
            APARM(5) = APARM(5) + 90.
            COS1TH = COS(APARM(5)*0.01745)
            SIN1TH = SIN(APARM(5)*0.01745)
            INTSA  = APARM(6)
            INTSA2 = APARM(7)
         ELSE
C                                       else no source component
            MAJA = 0
            END IF
C                                       reset for history file
         APARM(3) = MAJA
         APARM(4) = MINA
C                                       second source comp.
         X2SRC = BPARM(1)
         Y2SRC = BPARM(2)
C                                       major axis radius
         MAJB = ABS(BPARM(3))
         MINB = ABS(BPARM(4))
         IF (MINB .LE. 0. .OR. MINB .GE. NX/2) MINB = MAJB
C                                       if out of range, skip
         IF (MAJB .GT. 0. .AND. MAJB .LT. NX/2) THEN
            RMAJB = 1./(BPARM(3)*BPARM(3))
            RMINB = 1./(BPARM(4)*BPARM(4))
C                                       orient to Clockwise from N
            BPARM(5) = BPARM(5) + 90.
            COS2TH = COS(BPARM(5)*0.01745)
            SIN2TH = SIN(BPARM(5)*0.01745)
            INTSB  = BPARM(6)
            INTSB2 = BPARM(7)
         ELSE
C                                       else no second component
            MAJB = 0
            END IF
         BPARM(3) = MAJB
         BPARM(4) = MINB
C                                       third source comp.
         X3SRC = CPARM(1)
         Y3SRC = CPARM(2)
C                                       major axis radius
         MAJC = CPARM(3)
         MINC = CPARM(4)
         IF (MINC .LE. 0 .OR. MINC .GE. NX/2) MINC = MAJC
C                                       if out of range, skip
         IF (MAJC .GT. 0 .AND. MAJC .LT. NX/2) THEN
            RMAJC = 1./(CPARM(3)*CPARM(3))
            RMINC = 1./(CPARM(4)*CPARM(4))
C                                       orient to Clockwise from N
            CPARM(5) = CPARM(5) + 90.
            COS3TH = COS(CPARM(5)*0.01745)
            SIN3TH = SIN(CPARM(5)*0.01745)
            INTSC  = CPARM(6)
            INTSC2 = CPARM(7)
         ELSE
C                                       else no third component
            MAJC = 0
            END IF
         CPARM(3) = MAJC
         CPARM(4) = MINC
C                                       init image and source plane
         CALL INITAR ( NX, NY, QUASAR, IMAGES)
         NXO2 = NX/2
         NYO2 = NY/2
C                                       for all rows,  calculate
         DO 40 J = -NYO2,NY+NYO2
C                                       translate to galaxy centered
C                                       location in image plane
            YI = J - Y0
C                                       for all columns
            DO 30 I = -NXO2,NX+NXO2
C                                       translate to galaxy centered
C                                       location in image plane
               XI = I - X0
C                                       Determine Lens model
C                                       Point Mass model ?
               IF (LTYPE .EQ. POINT) THEN
C                                       Calculate magnification
                  CALL POINTL(XI, YI, B2, XS, YS, MAGI)
               ELSE
C                                       Else not spherically symetric
C                                       rotate to coords of galaxy
                  XPRM = (XI*COSTH) - (YI*SINTH)
                  YPRM = (XI*SINTH) + (YI*COSTH)
C                                       Blandford model ?
C                                       Calc jacobian of Bland Lens
                  IF (LTYPE .EQ. BLAND) THEN
                     CALL BLANDL (XPRM, YPRM, B0, EP0, S2,
     *                  XS, YS, MAGI)
C                                       Else, Devaucaleurs lens
C                                       de Vaucouleurs model
                  ELSE
                     CALL DEVLEN (XPRM, YPRM, B0, S0, XS, YS, MAGI)
                     END IF
C                                       rotate source pos back
                  XPRM =  (XS*COSTH) + (YS*SINTH)
                  YS   = (-XS*SINTH) + (YS*COSTH)
                  XS   = XPRM
C                                       end if point lens
                  END IF
C                                       absolute value of mag.
               IF (DPARM(7).LE.0.) MAGI = ABS(MAGI)
C                                       keep magnification
               MAGS = MAGI
C                                       translate to map coords
C                                       in source plane
               XS = XS + X0
               YS = YS + Y0
C                                       If mapping source plane mag.
               IF (SRCPLN.GE.0) THEN
C                                       convert to coord to integer
                  IXS = XS
                  IYS = YS
C                                       limit to valid coords
                  IF (IXS.GT.1 .AND. IXS.LE.NX-1 .AND.
     *                IYS.GT.1 .AND. IYS.LE.NY-1) THEN
C                                       for a few pixels around
                     DO 100 X = IXS-1, IXS+1
                        DO 100 Y = IYS-1, IYS+1
C                                       keep smallest abs magnitude
                           IF (ABS(MAGS) .LT. ABS(QUASAR(X,Y)))
     *                     QUASAR(X,Y) = MAGS
C                                       end for all near pixels
 100                       CONTINUE
C                                       end if coord in valid range
                     END IF
C                                       end if mapping source plane
                  END IF
C                                       if image plane map
               IF (I.GT.0 .AND. I.LE.NX .AND.
     *             J.GT.0 .AND. J.LE.NY .AND. SRCPLN .LE. 0) THEN
C                                       if mapping all magnification
                  IF (DPARM(9) .LT. 0. .OR. DPARM(9) .GT. 10.0) THEN
C                                       if greater magification
                     IF (ABS(MAGS) .LT. ABS(IMAGES(I,J)) .OR.
     *                   0.0       .EQ.     IMAGES(I,J) ) THEN
C                                       if not infinite magnifcation
                        IF (ABS(MAGS).LT. MAXMAG) MAGS = MAXMAG
                        IMAGES(I,J)  = 1.0/MAGS
                        END IF
                     END IF
C                                       add sources
C                                       if first component valid
                  IF (MAJA .GT. 0) THEN
                     DX = XS - X1SRC
                     DY = YS - Y1SRC
C                                       distance from source
                     CALL INELPS(DX,DY,RMAJA,RMINA,COS1TH,SIN1TH,DR2)
C                                       if in source, add flux to image
                     IF (DR2 .LE. 1.) THEN
                        IMAGES(I,J) = INTSA+(INTSA2*EXP(-3.*DR2))
                        END IF
C                                       end if first component
                     END IF
C                                       second source if valid
                  IF (MAJB .GT. 0) THEN
                     DX = XS - X2SRC
                     DY = YS - Y2SRC
C                                       distance from source
                     CALL INELPS(DX,DY,RMAJB,RMINB,COS2TH,SIN2TH,DR2)
C                                       if in source, add flux to image
                     IF (DR2 .LE. 1.) THEN
                        IMAGES(I,J) = INTSB+(INTSB2*EXP(-3.*DR2))
                        END IF
C                                       end if second source
                     END IF
C                                       third source if valid
                  IF (MAJC .GT. 0) THEN
                     DX = XS - X3SRC
                     DY = YS - Y3SRC
C                                       distance from source
                     CALL INELPS(DX,DY,RMAJC,RMINC,COS3TH,SIN3TH,DR2)
C                                       if in source, add flux to image
                     IF (DR2 .LE. 1.) THEN
                        IMAGES(I,J) = INTSC+(INTSC2*EXP(-3.*DR2))
                        END IF
C                                       end if third component
                     END IF
C                                       end if mapping source or image
                  END IF
C                                       end for all rows loop
 30            CONTINUE
 40         CONTINUE
C                                       Fix initalization
         CALL REIARS( NX, NY, QUASAR)
C                                       insert source if requested
         IF (DPARM(9).GT. 0) THEN
C                                       add each of the 3 components
            CALL ADDSRC ( NX, NY, APARM, QUASAR, IMAGES)
            CALL ADDSRC ( NX, NY, BPARM, QUASAR, IMAGES)
            CALL ADDSRC ( NX, NY, CPARM, QUASAR, IMAGES)
            MSGTXT = 'Adding Un-lensed source to map(s)'
            CALL MSGWRT(2)
            END IF
C                                       End if First execution
         END IF
C                                       Subsequent calls.
C                                       Read a row from stored arrays
      J = MIN( MAX( 1, IPOS(2)), MAXMAP)
C                                       get length of row to transfer
      NX = CATBLK(KINAX)
C                                       if outputing only source
      IF (SRCPLN .LT. 0) THEN
C                                       transfer a row
         DO 300 I = 1, NX
            RESULT(I) = IMAGES(I,J)
 300        CONTINUE
      ELSE
         IF (SRCPLN .GT. 0) THEN
C                                       transfer a row
            DO 400 I = 1, NX
               RESULT(I) = QUASAR(I,J)
 400           CONTINUE
         ELSE
C                                       Only half NX is filled
            NXO2 = MIN( MAX( 1, NX/2), MAXMAP)
C                                       output both source and image
            DO 500 I = 1, NXO2
               RESULT(I) = QUASAR(I,J)
               RESULT(NXO2 + I) = IMAGES(I,J)
               IF (RESULT(I) .GT. 1.E6 .AND. CNTMSG.LT.10) THEN
                  WRITE(MSGTXT,1000) 'SOURCE',I,J,QUASAR(I,J)
                  CALL MSGWRT(4)
                  CNTMSG = CNTMSG + 1
                  END IF
               IF (RESULT(NXO2 + I) .GT. 1.E6 .AND. CNTMSG.LT.10) THEN
                  WRITE(MSGTXT,1000) 'IMAGE ',I,J,IMAGES(I,J)
                  CALL MSGWRT(4)
                  CNTMSG = CNTMSG + 1
                  END IF
 500        CONTINUE
         END IF
      END IF
C                                       Finished
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT('ERROR IN ',A,' ARRAY(',I4,',',I4,')=',1PE12.3)
      END
      SUBROUTINE ADDSRC (NX, NY, ZPARM, QUASAR, IMAGES)
C-----------------------------------------------------------------------
C   Initializes the SOURCE and IMAGE ARRAYS
C   Input:
C    NX,NY    I   array sizes
C    ZPARM    R   position of source
C   Output
C    QUASAR and IMAGES
C             R   Source Plane and Image plane Jacobian arrays.
C-----------------------------------------------------------------------
      INTEGER   NX, NY
      REAL      ZPARM(10)
      INCLUDE 'GLENS.INC'
      REAL      IMAGES(MAXMAP,MAXMAP), QUASAR(MAXMAP,MAXMAP)
      INTEGER   I,J
      REAL      XSRC, YSRC, MAJ, MIN, TH, INTS, INTS2
      REAL      DR2, DX, DY, COSTH, SINTH, RMAJ2, RMIN2
C-----------------------------------------------------------------------
C                                       get source parameters, X,Y
      XSRC  = ZPARM(1)
      YSRC  = ZPARM(2)
C                                       size and angle
      MAJ   = ZPARM(3)
      MIN   = ZPARM(4)
      TH    = ZPARM(5)
C                                       two component source intensity
      INTS  = ZPARM(6)
      INTS2 = ZPARM(7)
C                                       if too big, skip
      IF (MAJ .LE. 0 .OR. MAJ .GE. NX/2) GO TO 999
      IF (MIN .LE. 0 .OR. MIN .GE. NX/2) MIN = MAJ
C                                       reciprocal axis squared
      RMAJ2 = 1./(MAJ*MAJ)
      RMIN2 = 1./(MIN*MIN)
C                                       convert to radians
      COSTH = COS(TH*0.01745)
      SINTH = SIN(TH*0.01745)
C                                       for all rows
      DO 44 J = 1,NY
         DY  = J-YSRC
C                                       for all columns
         DO 43 I = 1,NX
            DX  = I-XSRC
            CALL INELPS(DX,DY,RMAJ2,RMIN2,COSTH,SINTH,DR2)
C                                       if within source
            IF (DR2 .LE. 1.0) THEN
C                                       mark source with flux
               IMAGES(I,J) = INTS+((1.-DR2)*INTS2)
               QUASAR(I,J) = INTS+((1.-DR2)*INTS2)
               END IF
  43        CONTINUE
  44  CONTINUE
 999  RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE INELPS( DX, DY, RMAJ2, RMIN2, COSTH, SINTH, DR2)
C-----------------------------------------------------------------------
C  Determine squared distance DR2 from delta position DX, DY
C  for an ellipse with major and minor axies and a rotation.
C  Input
C   DX,DY         R   Delta: Coord - Center of ellipse
C   RMAJ2, RMIN2  R   reciprocal squared major and minor axies
C   COSTH, SINTH  R   Cos and Sin of ellipse rotation angle
C Output
C   DR2           R   distance from ellipse center, > 1 means outside
C
C  A point is inside an ellipse, when
C           2         2
C     (DX/A)  + (DY/B)   < 1.
C-----------------------------------------------------------------------
      REAL   DX,DY, RMAJ2, RMIN2, COSTH, SINTH
      REAL   DR2, DXPRM, DYPRM
C-----------------------------------------------------------------------
C                                            rotate x coord
      DXPRM = (DX*COSTH) - (DY*SINTH)
      DR2 = DXPRM*DXPRM*RMAJ2
C                                            if already outside, skip
      IF (DR2 .LE. 1.) THEN
C                                            rotate y coord
         DYPRM = (DX*SINTH) + (DY*COSTH)
C                                            calc distance squared
         DR2 = DR2 + (DYPRM*DYPRM*RMIN2)
         END IF
      RETURN
C-----------------------------------------------------------------------
      END
      SUBROUTINE INITAR (NX,NY,QUASAR,IMAGES)
C-----------------------------------------------------------------------
C   Initializes the SOURCE and IMAGE ARRAYS
C   Input:
C    NX,NY    I   array sizes
C   Output
C    QUASAR and IMAGES
C             R   Source Plane and Image plane Jacobian arrays.
C-----------------------------------------------------------------------
      INTEGER   NX, NY
      INTEGER   I,J
      INCLUDE 'GLENS.INC'
      REAL   IMAGES(MAXMAP,MAXMAP),QUASAR(MAXMAP,MAXMAP)
C-----------------------------------------------------------------------
C                                            for all rows
      DO 4 J = 1,NY
C                                            for all columns
         DO 3 I = 1,NX
C                                            initialize
            QUASAR(I,J) = 100.
            IMAGES(I,J) = 0.
  3      CONTINUE
  4   CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE REIARS (NX,NY,QUASAR)
C-----------------------------------------------------------------------
C   REInitializes the SOURCE AR, Replaceing large value with 0.
C   Input:
C    NX,NY    I   array sizes.
C   Output
C    QUASAR   R   Source Plane array.
C-----------------------------------------------------------------------
      INTEGER   NX, NY
      INTEGER   I,J
      INCLUDE 'GLENS.INC'
      INCLUDE 'EPSLN.INC'
      REAL   QUASAR(MAXMAP,MAXMAP)
C-----------------------------------------------------------------------
C                                       for all columns
      DO 4 J = 1,NY
C                                       for all rows
         DO 3 I = 1,NX
C                                       if source array not set
            IF (QUASAR(I,J) .EQ. 100.) THEN
C                                       reset to no magnification
               QUASAR(I,J) = 0.
            ELSE
C                                       else if mag. not infinite
               IF (ABS(QUASAR(I,J)) .GT. MAXMAG) THEN
C                                       calc mag from inverse mag
                  QUASAR(I,J) = 1.0/QUASAR(I,J)
               ELSE
C                                       else limit magnification
                  QUASAR(I,J) = 1.0/MAXMAG
                  END IF
C                                       end if mag set
               END IF
C                                       end for all rows
  3      CONTINUE
  4   CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE BLANDL (XI, YI, B0, EP0, S2, XS, YS, MAG)
C-----------------------------------------------------------------------
C   Calculates the jacobian of the Kochanek, Blandford galaxy model.
C   Graitational lens magnification.
C   Input:
C    XI,YI    R   coordinate of Image point for calculation
C    B0       R   Einstein Ring size in pixels
C    EP0      R   projected eccentricity of the galaxy
C    S2       R   core size parameter SQUARED
C Output
C    XS,YS    R   coordinates of source plane
C    Mag      R   Reciprocal Magnifation of flux falling at location X,Y
C-----------------------------------------------------------------------
      REAL      XI, YI, B0, EP0, S2, MAG, XS, YS
      REAL      X2, Y2, X2A
      REAL      PHI, PHI2, B0PHI, B0PHI3
      INCLUDE 'EPSLN.INC'
C --------------------------------------------------------------
      Y2 = YI*YI
      X2 = XI*XI
C                                        Do not allow x=y=0
      X2A = X2
      IF (X2+Y2 .EQ. 0.0) X2A = 1.0
C                                        Calculate the radius
      PHI2 = S2 + ((1.-EP0)*X2A) + ((1.+EP0)*Y2)
C                                        Avoid div 0
      IF (PHI2.GE.EPSLN2) THEN
         PHI = SQRT(PHI2)
      ELSE
         PHI = EPSLN
      END IF
C                                        Usefull powers of PHI
      B0PHI  = B0/PHI
      B0PHI3 = B0PHI/(PHI*PHI)
C                                        Spherical Approx
      MAG = 1. - B0PHI
C                                        independent of S0
      MAG = MAG + (B0PHI3*EP0 *( (Y2-X2A)+(EP0*(X2A+Y2)) ))
C                                        dependent on S2
      MAG = MAG - (B0PHI3*S2 *( 1.-(B0PHI*(1.-(EP0*EP0))) ))
C                                            determin source location
      XS = XI - (XI*B0PHI*(1.-EP0))
      YS = YI - (YI*B0PHI*(1.+EP0))
999   RETURN
      END
      SUBROUTINE DEVLEN (XI, YI, B0, S0, XS, YS, MAG)
C-----------------------------------------------------------------------
C   Calculates the magnification of a de Vauccouleurs galaxy model.
C   Graitational lens magnification.
C   Input:
C    XI,YI    R   coordinate of image point for calculation
C    B0       R   galaxy mass in pixels
C    S0       R   core size parameter
C Output
C    XS,YS    R   coordinates of source
C    Mag      R   Magnifation of flux falling at location X,Y
C-----------------------------------------------------------------------
      REAL      XI, YI, B0, S0, MAG, XS, YS
      REAL      X2, Y2, X2A, R, R2, R4
      REAL      ETA, MASS
      INCLUDE 'EPSLN.INC'
C --------------------------------------------------------------
      Y2 = YI*YI
      X2 = XI*XI
C                                        Do not allow x=y=0
      X2A = X2
      IF (X2+Y2 .EQ. 0.0) X2A = 1.0
C                                        Calculate the radius
      R2 = X2A + Y2
C                                        Avoid div 0
      IF (R2.GE.EPSLN2) THEN
         R = SQRT(R2)
      ELSE
         R = EPSLN
      END IF
C                                        Usefull powers of R
      R2 = R*R
      R4 = R2*R2
      ETA = SQRT(SQRT(R/S0))
C234567                                  Spherical Approx
       MASS = B0*B0*(1. - EXP(-ETA)*
     *             (1. + ETA*(1. + 0.5*ETA*
     *               (1. + 0.333333*ETA*
     *                 (1. + 0.25*ETA*
     *                   (1. + 0.2*ETA*
     *                     (1. + 0.1666666*ETA*(1. + 0.1428*ETA)
     *           ) ) ) ) ) )  )
C                                        Spherical Approx
      MAG = (R4 - (MASS*MASS))/R4
C                                            determin source location
      XS = XI - (XI*MASS/R2)
      YS = YI - (YI*MASS/R2)
999   RETURN
      END
      SUBROUTINE POINTL (XI, YI, B2, XS, YS, MAG)
C-----------------------------------------------------------------------
C   Calculates the jacobian of the point mass lens model.
C   Graitational lens magnification.
C   Input:
C    XI,YI    R   coordinate of point in IMAGE plane for calculation
C                 Coordinate system is centered on the lens
C    B2       R   Einstein Ring size squared in pixels
C Output
C    XS,YS    R   coordinate of point in SOURCE Plane
C    Mag      R   Reciprocal Magnifation of flux at location X,Y
C For the sphically symetric point lens, all images are deflected
C radially.  The source X and Y location are calculated from the
C ratio of radial positions.  RS/RI = (RI2 - B2)/RI2
C-----------------------------------------------------------------------
      REAL      XI, YI, B2, XS, YS, MAG
      REAL      X2, Y2, RI2, RI4, X2A, RATIO
      INCLUDE 'EPSLN.INC'
C --------------------------------------------------------------
      X2 = XI*XI
      Y2 = YI*YI
C                                        Do not allow x=y=0
      X2A = X2
      IF (X2 .EQ. 0.0 .AND. Y2 .EQ. 0.0) X2A = 1.0
C                                        Calculate the radius
      RI2 = X2A + Y2
C                                        Guard against div 0
      IF (RI2.LT.EPSLN2) RI2 = EPSLN2
      RI4 = RI2*RI2
C                                        Source plane positions
C Delta Theta = b2/r;  cos phi = x/r;  delta X = Delta Theta * Cos phi
C
      RATIO = (RI2 - B2) / RI2
      XS  = XI * RATIO
      YS  = YI * RATIO
C                                        The inverse magnifcation is
C                                        calculated from image radius
      MAG = (RI4 - (B2*B2))/RI4
      RETURN
      END
      SUBROUTINE NARAYN (XI, YI, B0, RCORE3, XS, YS, MAG)
C-----------------------------------------------------------------------
C   Calculates the jacobian of the narayan elliptical lens model.
C   Graitational lens magnification.
C   Input:
C    XI,YI    R   coordinate of image point for calculation
C    B0       R   Einstein Ring size squared in pixels
C Output
C    Mag      R   Magnifation of flux falling at location X,Y
C-----------------------------------------------------------------------
      REAL      XI, YI, B0, XS, YS, MAG, RCORE3
      REAL      X2, Y2, R, R2, X2A, RCORE, RADLF
      INCLUDE 'EPSLN.INC'
C --------------------------------------------------------------
      X2 = XI*XI
      Y2 = YI*YI
C                                        Do not allow x=y=0
      X2A = X2
      IF (X2 .EQ. 0.0 .AND. Y2 .EQ. 0.0) X2A = 1.0
C                                        Calculate the radius
      R2 = X2A + Y2
      R = 0.
      IF (R2.GE.0.) R = SQRT(R2)
C                                        Guard against div 0
      IF (R.LT.EPSLN) R = EPSLN
      R2 = R*R
      RCORE = B0
C
      RCORE = B0
C                                        outside core Radius ?
      IF (R .GT. RCORE) THEN
C                                        Spherical Approx
         RADLF = (-1.0/R) + (0.375*B0/R2)
      ELSE
         RADLF = (-.75/RCORE) + (0.125*R2/RCORE3)
      END IF
      MAG = RADLF * B0
C                                            determin source location
      XS = XI - (XI*MAG)
      YS = YI - (YI*MAG)
999   RETURN
      END
