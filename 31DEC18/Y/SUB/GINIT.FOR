      SUBROUTINE GINIT (IVOL, ICNO, PNAME, IGSIZE, ITYPE, NPARM, PARMS,
     *   DOTV, TVGREY, TVGRPH, TVCORN, CATBLK, BUFF, LUN, FIND, IERR)
C-----------------------------------------------------------------------
C! creates, opens, initializes plot file or TV (does not catalog it)
C# Plot-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2002-2003, 2008, 2010-2012, 2016, 2018
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
C   GINIT initializes the graphics common, creates and opens the
C   graphics file, and places the "initialize plot" command in the file.
C   ********  NOTE: cataloging of the graphics file must be handled by
C   the calling program.  *********************
C   GINIT writes 1 or more records to disk containing the task name and
C   parameters.  It starts on the next physical record with:
C   The command record has the form:
C      I       opcode (1)
C      I       user number
C      I(3)    date:  19yy, mm, dd
C      I       ITYPE: type code 1 => misc.,  2 => CNTR,   3 => greys,
C                  4 => PROFL,  5 => SL2PL,  6 => pcntr,  7 => IMEAN,
C                  8 => UVPLT,  9 => GNPLT, 10 => VBPLT, 11 => PFPLn,
C                 12 => GAPLT, 13 => PLCUB, 14 => IMVIM, 15 => TAPLT,
C                 16 => POSSM, 17 => SNPLT, 18 => kntr,  19 => UVHGM,
C                 20 => ISPEC, 21 => VPLOT, 22 => CLPLT, 23 => DFTPL,
C                 24 => FRPLT, 25 => FRMAP, 26 => PLOTR, 27 => GREYS,
C                 28 => PCNTR, 29 => KNTR,  30 => BPLOT, 31 => APCAL,
C                 32 => ANBPL, 33 => IRING  34 => RSPEC, 35 => CAPLT
C                 36 => RFLAG, 37 => CCNTR, 38 => SERCH, 39 => EVAUV,
C                 40 => DELZN, 41 => ELINT, 42 => FGPLT, 43 => GAL,
C                 44 => SNIFS, 45 => SOUSP, 46 => WETHR, 47 => RM2PL,
C                 48 => XG2PL, 49 => ALVAR, 50 => PCPLT, 51 => PLROW,
C                 52 => PLOTC, 53 => UVHOL, 54 => PBEAM, 55 => PEEK,
C                 56 => LOCIT
C                 where lower case -> obsolete version
C   WARNING: Get PNAME right!  If the create fails because the file
C   already exists, GINIT will destroy PNAME and create the desired
C   plot file.
C   Alternatively, GINIT starts the process of writing the plot
C   directly to the TV
C   Inputs:
C      IVOL    I         disk volume for file creation
C      ICNO    I         catalog number
C      PNAME   C*48      physical file name for created file
C      IGSIZE  I         0 => small 1 => medium 2 => large file
C      ITYPE   I         plot type code (if < 0, then ITYPE is taken to
C                        be #rows (# cols if writing vertically) and is
C                        used to adjust SCALEF to integer*iabs(itype)
C                        The output ITYPE is set to 1.
C      NPARM   I         # (f.p.) values in input parameter list
C      PARMS   R(NPARM)  task input parameter list (with defaults
C                        filled in)
C      DOTV    L         Selects use of TV (True) or plot file (False)
C      TVGREY  I         Grey channel to use (0 => 1)
C      TVGRPH  I         TV Graphics channel to use (0 => 1)
C      TVCORN  I(2)      X,Y TV pixel to put plot origin (0 => center)
C      CATBLK  I(256)    Image header
C   In/out:
C      BUFF    I(256)    plot commands are placed in this buffer and
C                        written to disk as needed.  The calling program
C                        must not alter this area between calls to GINIT
C                        and GFINIS
C   Outputs:
C      LUN     I         logical unit number of plot file
C      FIND    I         location in FTAB for file
C      IERR    I         error code: 0 => ok
C                           1 => another plot file open
C                           2 => input data error
C                           3 => create error (no file)
C                           4 => open error (file gone)
C-----------------------------------------------------------------------
      CHARACTER PNAME*48
      INTEGER   IVOL, IGSIZE, ITYPE, BUFF(256), LUN, NPARM, FIND, IERR,
     *   ICNO, TVGREY, TVGRPH, TVCORN(2), CATBLK(256)
      LOGICAL   DOTV
      REAL      PARMS(*)
C
      INTEGER   LUNGPH, IFIND, OPCODE, IGUESS(3), IBITS, IG, I, IER,
     *   ISIZE, LSIZE, IP, NP
      REAL      ROUND
      LOGICAL   T, F
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DTVC.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA OPCODE /1/,              LUNGPH /26/
      DATA IGUESS / 84, 420, 1260/
C-----------------------------------------------------------------------
      GPHERV = 0
      GPHERC = 0
      GPHERG = 0
      GPHERO = 0
      GPHPLX(1) = 0
      GPHPLX(2) = 0
      GPHPLY(1) = 0
      GPHPLY(2) = 0
      GPHDOT = DOTV
      GPHPAG = .FALSE.
      GPHX1 = 0.0
      GPHX2 = 1.0
      GPHY1 = 0.0
      GPHY2 = 1.0
      GPHTLO = 0
      GPHTHI = 0
      GPHLTY = 0
      GPHCUT = 0.33333
      GPHDOD = .TRUE.
      GPHFUN = 'LN'
      IBITS = MIN (32, NBITWD) - 4
      SCALEF = 2.0 ** IBITS   - 2.0
      ROUND = ABS(ITYPE)
      IF (ITYPE.LT.0) SCALEF = AINT(SCALEF/ROUND) * ROUND
      SCALEF = AINT (SCALEF+0.5)
      GPHCNT = 0
      GPHCOL(1) = 0.0
      GPHCOL(2) = 0.0
      GPHCOL(3) = 0.0
C                                       Plot file
      IF (.NOT.DOTV) THEN
C                                        another plot file open?
         CALL LSERCH ('SRCH', LUNGPH, IFIND, F, IER)
         IF (IER.EQ.1) CALL LSERCH ('SRCH', LUNGPH, IFIND, T, IER)
         IF (IER.EQ.1) GO TO 10
            IERR = 1
            WRITE (MSGTXT,1000)
            GO TO 990
C                                        create file
 10      IG = IGSIZE + 1
         IG = MIN (3, MAX (1, IG))
         IG = IGUESS (IG)
         ISIZE = IG
         I = 0
 15      MSGSUP = 32000
         CALL ZCREAT (IVOL, PNAME, ISIZE, F, LSIZE, IER)
         MSGSUP = 0
         I = I + 1
         IF (IER.EQ.0) GO TO 25
C                                        Destroy existing file
            IF ((I.GT.1) .OR. (IER.NE.1)) GO TO 20
               WRITE (MSGTXT,1015)
               CALL MSGWRT (6)
               CALL ZDESTR (IVOL, PNAME, IER)
               IF (IERR.LE.1) GO TO 15
                  WRITE (MSGTXT,1016) IER
                  IERR = 3
                  GO TO 990
 20         CONTINUE
               IERR = 2
               IF (IER.GT.2) IERR = 3
               WRITE (MSGTXT,1020) IER
               IF (IER.EQ.3) WRITE (MSGTXT,1021)
               IF (IER.EQ.5) WRITE (MSGTXT,1022) IVOL
               GO TO 990
C                                        open file
 25      CALL ZOPEN (LUNGPH, IFIND, IVOL, PNAME, F, T, T, IER)
         IERR = MIN (IER, 4)
         IF (IER.EQ.0) GO TO 30
            WRITE (MSGTXT,1025) IER
            IF (IER.EQ.2) IERR = 3
            IF (IER.EQ.3) IERR = 2
            GO TO 980
C                                        build common
 30      GPHSIZ = LSIZE
         GPHLUN = LUNGPH
         GPHIND = IFIND
         GPHPOS = 0
         GPHRRN = 0
         GPHVOL = IVOL
         GPHNAM = PNAME
C                                        return answers
         LUN = GPHLUN
         FIND = GPHIND
         CALL FILL (256, 0, BUFF)
C                                       fill in record 1
         CALL CHR2H (6, TSKNAM, 1, BUFF(1))
         CALL ZDATE (BUFF(4))
         CALL ZTIME (BUFF(7))
         BUFF(10) = NPARM
         I = NPARM
         IP = 11
         IG = 1
 50      NP = MIN (I, 257-IP)
         IF (NP.GT.0) THEN
            CALL COPY (NP, PARMS(IG), BUFF(IP))
            CALL GPHWRT (BUFF, IER)
            I = I - NP
            IP = 1
            IG = IG + NP
            GO TO 50
            END IF
C                                        fill in record 2 (or n)
         BUFF(GPHPOS+1) = OPCODE
         BUFF(GPHPOS+2) = NLUSER
         BUFF(GPHPOS+6) = ITYPE
         IF (ITYPE.LT.0) BUFF(GPHPOS+6) = 1
         CALL ZDATE (BUFF(GPHPOS+3))
         GPHPOS = GPHPOS + 6
C                                       TV
      ELSE
C                                       open the TV
         CALL TVOPEN (GPHCAT, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
         GPHCRN(1) = TVCORN(1)
         GPHCRN(2) = TVCORN(2)
         IF (TVGRPH.GT.0) THEN
            I = TVGRPH + NGRAY
            CALL FILL (7, I, GPHTVG)
         ELSE
            DO 60 I = 1,7
               GPHTVG(I) = I + NGRAY
 60            CONTINUE
            END IF
         GPHTVG(8) = 8 + NGRAY
         CALL COPY (256, CATBLK, GPHCAT)
         CALL COPY (4, WINDTV, GPHTVW)
         GPHTVC(1) = TVGREY
         IF ((GPHTVC(1).LT.1) .OR. (GPHTVC(1).GT.NGRAY)) GPHTVC(1) = 1
         GPHTVC(2) = MIN (GPHTVC(1)+1, NGRAY)
         GPHTVC(3) = MIN (GPHTVC(2)+1, NGRAY)
         IF ((GPHCRN(1).LT.1) .OR. (GPHCRN(1).GT.MAXXTV(1)))
     *      GPHCRN(1) = 0
         IF ((GPHCRN(2).LT.1) .OR. (GPHCRN(2).GT.MAXXTV(2)))
     *      GPHCRN(2) = 0
         GPHNGR = 0
         GPHCAT(IIVOL) = IVOL
         GPHCAT(IICNO) = ICNO
         GPHCAT(IIPLT) = MAX (1, ITYPE)
         IF ((GPHCRN(1).NE.0) .AND. (GPHCRN(2).NE.0)) THEN
            GPHTVW(1) = 1
            GPHTVW(2) = 1
            GPHTVW(3) = MAXXTV(1)
            GPHTVW(4) = MAXXTV(2)
            END IF
         CALL YHOLD ('ONNN', IERR)
         CALL FILL (32, 0, GPHCON)
         END IF
      GO TO 999
C                                       TV error
 960  CALL TVCLOS (GPHSCR, IER)
      WRITE (MSGTXT,1960) IERR, TVSUBR
      GO TO 990
C                                       ERROR: destroy file
 980  CALL ZDESTR (IVOL, PNAME, IER)
C                                       message
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GINIT: ANOTHER PLOT FILE ALREADY OPEN')
 1015 FORMAT ('WARNING: REPLACING EXISTING (UNKNOWN ?) FILE')
 1016 FORMAT ('FAILED TO REMOVE EXISTING FILE.  ERROR',I7)
 1020 FORMAT ('GINIT: PLOT FILE NOT CREATED: ZCREAT ERROR',I7)
 1021 FORMAT ('GINIT: PLOT FILE NOT CREATED: NO ROOM ON DISK')
 1022 FORMAT ('GINIT: PLOT FILE NOT CREATED: PROHIBITED ON DISK',I4)
 1025 FORMAT ('GINIT: PLOT FILE NOT CREATED: ZOPEN ERROR',I7)
 1100 FORMAT ('GINIT: ERROR',I5,' OPENING THE TV')
 1960 FORMAT ('GINIT: ERROR',I5,' FROM TV ROUTINE ',A)
      END
