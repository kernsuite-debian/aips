      PROGRAM SUMIM
C-----------------------------------------------------------------------
C! Task to add several maps to form an output map
C# Map-util Spectral Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998, 2008-2010
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
C   SUMIM is an AIPS system task to add a set of maps described by
C   the same name & class and by sequence numbers from n1 thru n2 by n3
C   For meaningful results, the maps must be precisely registered.
C   Inputs: adverbs from AIPS -
C   USERID             Input map user ID
C   INNAME(3)          Input map name
C   INCLASS(2)         Input map class
C   INSEQ              First input map seq no.
C   IN2SEQ             Seq # of last map to add
C   IN3SEQ             Increment of seq #s for maps to add
C   INDISK             Input map disk: 0 = any
C   OUTNAME(3)         Output map name default INNAME
C   OUTCLASS(2)        Output map class default INCLASS
C   OUTSEQ             Output map seq no. 0 => lowest unique
C   OUTDISK            Output map disk
C   BLC(7)             Bottom left corner all input maps
C   TRC(7)             Top right corner all input maps
C   FACTOR             Multiplier for added maps: 0 => 1 / (# summed)
C   BADDISK(10)        Disks to avoid for scratch
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PMAD.INC'
      CHARACTER PRGNAM*6, NAMS(4)*36, SUBS(7)*6, CT12*12,
     *   CT6*6, CT2*2
      HOLLERITH XRPARM(42), PTYPE
      REAL      RPARM(42), IN(MAXIMG), OUT(MAXIMG), WT, TEMP
      LOGICAL   T, EQUAL
      INTEGER   ERROR, NSUB, JFIL, IROUND, IT1, IT2, IT3, CAT2(256),
     *   NRET, EOM, ASEQ, LMAP, LUNSL(2), HLUN(2), INVOL,
     *   INCNO, OUTVOL, OUTCNO, S1,
     *   S2, S3, NPARM, I, J, LX, I2, I1, IERR
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DITB.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DBUF.INC'
      EQUIVALENCE (XRPARM, RPARM)
      DATA PRGNAM /'SUMIM '/
      DATA LMAP, LUNSL, HLUN /17, 18,19, 27,28/
      DATA EOM /9/
      DATA T /.TRUE./
      DATA SUBS /'GTPARM', 'OPENCF',
     *           'GETHDR', 'MAPCR ',
     *           'MAPIO ', 'MAPCOP', 'WINDOW'/
C-----------------------------------------------------------------------
C                                       Task init, get parms
      NRET = 0
      NPARM = 41
      NSUB = 1
      JFIL = 1
      CALL TSKBEG (PRGNAM, NPARM, RPARM(2), ERROR)
      IF (ERROR.NE.0) GO TO 980
      RPARM(1) = NLUSER
C                                       Set up file NAMEstrings for
C                                       input & output maps & 2
C                                       scratch files.
C                                       First: input file
      CALL CHR2H (2, 'MA', 1, PTYPE)
      CALL H2WAWA (XRPARM(2), XRPARM(5), RPARM(7), PTYPE, RPARM(10),
     *   RPARM(1), NAMS(1))
C                                       Then: output files
      CALL H2WAWA (XRPARM(11), XRPARM(14), RPARM(16), PTYPE, RPARM(17),
     *   RPARM(1), NAMS(2))
C                                       Then: scratch files
      CALL CHR2H (2, 'SC', 1, PTYPE)
      J = IROUND (RPARM(10))
      DO 5 I = 3,4
         TEMP = MOD(J+I-3, NVOL) +1
         CALL H2WAWA (XRPARM(11), XRPARM(14), REAL(I), PTYPE, TEMP,
     *      RPARM(1), NAMS(I))
 5       CONTINUE
      DO 15 I = 1,10
         IBAD(I) = IROUND (RPARM(32+I))
 15      CONTINUE
C                                       Open first input map to get
C                                       header, then close again
      NSUB = 2
      CALL OPENCF (LMAP, NAMS(1), ERROR)
      IF (ERROR.NE.0) GO TO 980
      NSUB = 3
      CALL GETHDR (LMAP, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 980
      INVOL = FILTAB(POVOL,6)
      INCNO = FILTAB(POCAT,6)
      CALL FILCLS (LMAP)
C                                       Messages
      WRITE (MSGTXT,1010)
      CALL MSGWRT (3)
      CALL PRTNAM (NAMS(1), 3)
C                                       Create R   scratch files
      NSUB = 7
      CALL COPY (256, CATBLK, CAT2)
      CALL HDRWIN (RPARM(18), RPARM(25), CAT2, ERROR)
      IF (ERROR.NE.0) GO TO 980
      NSUB = 4
      DO 20 J = 3,4
         JFIL = J
         CALL MAPCR (NAMS(1), NAMS(J), CAT2, ERROR)
         IF (ERROR.NE.0) GO TO 980
 20      CONTINUE
C                                       Zero the 1st scratch file
      LX = CAT2(KINAX)
      CALL RFILL (LX, 0, OUT)
      NSUB = 2
      JFIL = 3
C                                       Open file
      CALL OPENCF (LUNSL(1), NAMS(3), ERROR)
      IF (ERROR.NE.0) GO TO 980
      NSUB = 5
      JFIL = 3
C                                       Loop to write zeros in file
 100  CALL MAPIO ('WRIT', LUNSL(1), OUT, ERROR)
         IF (ERROR.EQ.0) GO TO 100
         IF (ERROR.NE.EOM) GO TO 980
C                                       Open second scratch file
      NSUB = 2
      JFIL = 4
      CALL OPENCF (LUNSL(2), NAMS(4), ERROR)
      IF (ERROR.NE.0) GO TO 980
C                                       Set up I   looping indicies
      S1 = IROUND (RPARM(7))
      S2 = IROUND (RPARM(8))
      S3 = IROUND (RPARM(9))
      IF (S2.GE.S1) GO TO 110
         I = S2
         S2 = S1
         S1 = I
         S3 = -S3
 110  IF (S3.LE.0) S3 = 1
      WT = RPARM(32)
      IF (WT.NE.0.0) GO TO 160
         I = 1 + ((S2 - S1)/S3)
         WT = 1.0 / I
 160  I2 = 1
C                                       Loop over input maps
      DO 300 I = S1,S2,S3
C                                       Switch scratch files
         I1 = I2
         I2 = 3 - I2
C                                       All this pissing about to
C                                       say NAMS(6,1) = I
         CALL WAWA2A (NAMS(1), CT12, CT6, IT1, CT2, IT2, IT3)
         IT1 = I
         CALL A2WAWA (CT12, CT6, IT1, CT2, IT2, IT3, NAMS(1))
C                                       Open input map
         NSUB = 2
         JFIL = 1
         CALL OPENCF (LMAP, NAMS(1), ERROR)
         IF (ERROR.NE.0) GO TO 980
         WRITE (MSGTXT,1160) I
         CALL MSGWRT (2)
C                                       Check dimensions
         NSUB = 7
         CALL MAPWIN (LMAP, RPARM(18), RPARM(25), ERROR)
         IF (ERROR.NE.0) GO TO 980
         NSUB = 5
         CALL COMPAR (CATBLK(KIDIM), CATBLK(KINAX), CAT2(KINAX),
     *      EQUAL)
         IF (EQUAL) GO TO 200
            NSUB = 2
            ERROR = 2
            WRITE (MSGTXT,1161)
            CALL MSGWRT (8)
            GO TO 980
C                                       Read line loop
 200     CONTINUE
            JFIL = 1
C                                       Read an input line
            CALL MAPIO ('READ', LMAP, IN, ERROR)
C                                       On End of Map skip to loop end
            IF (ERROR.EQ.EOM) GO TO 290
            IF (ERROR.NE.0) GO TO 980
C                                       Read present accumlator file
            JFIL = 2 + I1
            CALL MAPIO ('READ', LUNSL(I1), OUT, ERROR)
            IF (ERROR.NE.0) GO TO 980
C                                       Add new data to old
            DO 250 J = 1,LX
               IF (IN(J).EQ.INDEF) OUT(J) = INDEF
               IF (OUT(J).NE.INDEF) OUT(J) = OUT(J) + WT * IN(J)
 250           CONTINUE
C                                       Write into new accumlator file
            JFIL = 2 + I2
            CALL MAPIO ('WRIT', LUNSL(I2), OUT, ERROR)
            IF (ERROR.NE.0) GO TO 980
            GO TO 200
C                                       Map loop; close input map
 290     CALL FILCLS (LMAP)
 300     CONTINUE
C                                       Close scratch files
      CALL FILCLS (LUNSL(I1))
      CALL FILCLS (LUNSL(I2))
C                                       Convert R   scratch file
C                                       to I   output file
      NSUB = 6
      WRITE (MSGTXT,1300)
      CALL MSGWRT (2)
      CALL MAPCOP (NAMS(2+I2), NAMS(1), NAMS(2), ERROR)
      IF (ERROR.NE.0) GO TO 980
C                                       Open output file to get
C                                       header, then close
      NSUB = 2
      CALL OPENCF (LMAP, NAMS(2), ERROR)
      IF (ERROR.NE.0) GO TO 330
      NSUB = 3
      CALL GETHDR (LMAP, CATBLK, ERROR)
      IF (ERROR.NE.0) GO TO 330
      OUTVOL = FILTAB(POVOL,6)
      OUTCNO = FILTAB(POCAT,6)
      CALL FILCLS (LMAP)
C                                       copy most keywords
      CALL KEYPCP (INVOL, INCNO, OUTVOL, OUTCNO, 0, ' ', IERR)
C                                       Create history file and
C                                       copy HI of INSEQ
 310  CALL HIINIT (3)
      CALL HISCOP (HLUN(1), HLUN(2), INVOL, OUTVOL, INCNO, OUTCNO,
     *   CATBLK, WBUFF, IBUF, ERROR)
      IF (ERROR.GT.3) GO TO 330
      IF (ERROR.EQ.3) GO TO 320
C                                       Add SUMIM history
      CALL WAWA2A (NAMS(1), CT12, CT6, IT1, CT2, IT2, IT3)
      ASEQ = IT1
      CALL HENCO1 (PRGNAM, CT12, CT6, ASEQ, IT2, HLUN(2),
     *   IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      CALL WAWA2A (NAMS(2), CT12, CT6, IT1, CT2, IT2, IT3)
      ASEQ = IT1
      CALL HENCOO (PRGNAM, CT12, CT6, ASEQ, IT2, HLUN(2),
     *   IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (MSGTXT,1310) PRGNAM, S1, S2, S3
      CALL HIADD (HLUN(2), MSGTXT, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (MSGTXT,1311) PRGNAM, WT
      CALL HIADD (HLUN(2), MSGTXT, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (MSGTXT,1312) PRGNAM, (RPARM(I), I = 18,24)
      CALL HIADD (HLUN(2), MSGTXT, IBUF, ERROR)
      IF (ERROR.NE.0) GO TO 320
      WRITE (MSGTXT,1313) PRGNAM, (RPARM(I), I = 25,31)
      CALL HIADD (HLUN(2), MSGTXT, IBUF, ERROR)
C                                       Close HI file
 320  CALL HICLOS (HLUN(2), T, IBUF, IERR)
C                                       Report any errors
 330  IF (ERROR.EQ.0) ERROR = IERR
      IF (ERROR.NE.0) THEN
         WRITE (MSGTXT,1330) ERROR
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                        Copy tables
      CALL ALLTAB (1, 'CC', HLUN(1), HLUN(2), INVOL, OUTVOL, INCNO,
     *   OUTCNO, CATBLK, WBUFF, IBUF, IERR)
      IF (IERR.GT.2) THEN
         MSGTXT = 'ERROR COPYING TABLE FILES'
         CALL MSGWRT (6)
         END IF
      GO TO 990
C                                       Print error message
 980  WRITE (MSGTXT,1980) SUBS(NSUB), ERROR
      CALL MSGWRT (8)
C                                       Print offending file
      CALL PRTNAM (NAMS(JFIL), 8)
      NRET = 16
C                                       go home
 990  CALL TSKEND (NRET)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('Input map')
 1160 FORMAT ('Open map with seq no',I5)
 1161 FORMAT ('But its axes do not match the output')
 1300 FORMAT ('Copy scratch to output image')
 1310 FORMAT (A6,'/ seq. number from',I4,' to',I4,' by',I4)
 1311 FORMAT (A6,' factor = ',1PE13.5,10X,'/ multiplier')
 1312 FORMAT (A6,' BLC =',6(F5.0,','),F5.0)
 1313 FORMAT (A6,' TRC =',6(F5.0,','),F5.0)
 1330 FORMAT ('WARNING: HI FILE HAD PROBLEMS.  ERROR=',I7)
 1980 FORMAT (A6,' ERROR NO ',I6)
         END
