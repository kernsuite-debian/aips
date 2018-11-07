LOCAL INCLUDE 'AFILE.INC'
      INTEGER   NUMLIN
      PARAMETER (NUMLIN=20000)
C
      HOLLERITH XINFIL(12), XOUTFL(12), XOPCOD
      CHARACTER INFIL*48, OUTFIL*48, OPCODE*4
      REAL      APARM(10)
      INTEGER   LUN1, LUN2, FIND1, FIND2, BUFF1(256), TOTLIN,
     *   EXPNUM
      CHARACTER LINE(NUMLIN)*256, TIT1*256, TIT2*256
      REAL      TIMIND(NUMLIN)
      INTEGER   BASIND(NUMLIN), AINDEX(NUMLIN)
      COMMON /INPARM/ XINFIL, XOUTFL, XOPCOD, APARM
      COMMON /AFILP/ LUN1, LUN2, FIND1, FIND2, TIMIND, BASIND, BUFF1,
     *   AINDEX, TOTLIN, EXPNUM
      COMMON /CHARPM/ INFIL, OUTFIL, OPCODE
      COMMON /CHDATA/ LINE, TIT1, TIT2
LOCAL END
      PROGRAM AFILE
C-----------------------------------------------------------------------
C! Task to sort and edit a MkIII correlator A-file.
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2012, 2015
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
C   AFILE allows a user to provide a subroutine which performs an
C   operation on a UV data base, writing an output UV data base.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INFILE         INFIL         Input text A-file
C      OUTFILE        OUTFIL        Output text A-file
C      APARM(10)      APARM         A-FILE selection criteria
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'AFILE.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
C
      DATA PRGM /'AFILE '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
      CALL AFILIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE AFILIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   AFILIN gets input parameters for AFILE.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C
C
C
C  The doumentation of the format is as follows:
C
C A-FILE FORMAT
C
C General rules:
C   1. Any line with a '*' in column 1 is a comment.
C   2. Each field in a data line is separated by one or more spaces.
C   3. A-files are all named #Axxxx where xxxx is a serial# assigned
C      in order of generation.  Each A-file acts as a summary and index
C      for the corresponding set of "A" and "B" archive tapes.
C
C OLD FORMAT: (up through and including A0918):
C
C  1. File name  (6-chars)
C  2. Extent type  (2-chars)
C  3. Extent#  (2-chars)
C  4. #blocks in extent (4-chars)  1 block=256 bytes
C  5. Expt serial#  (3 chars)   (Haystack-assigned)
C  6. Processing date  (10 chars)
C  7. Year of scan  (2-chars)
C  8. Scan ID  (8-chars, in format DDD-HHMM of scheduled start time)
C  9. Source name  (8-chars)
C 10. Baseline  (2-chars)
C     FRNGE quality code (1-char) See below.
C 11. Reference tape#  (8-chars)
C 12. Remote tape#  (8-chars)
C 13. Frequency code  (1-char)
C     #frequencies processed by FRNGE  (2-chars)
C 14. Correlation amplitude  (F5.1, in units of 1.0E-04)
C 15. SNR  (4-chars)
C 16. Residual phase  (Earth centered)  (degrees F5.1)
C 17. Residual single-band delay  (usec F5.2)
C 18. Residual multi-band delay  (usec F7.4)
C 19. Residual delay rate  (psec/sec F7.2)
C 20. Tape quality codes  (6-chars)  See below.
C 21. "A-file#"  (5-chars)
C
C     Total of 131 characters on line.
C
C  NEW FORMAT (from A0919):
C
C Field#
C  1. File name  (6-chars)
C  2. Extent type (2-chars) 50=ROOT 51=COREL 52=FRNGE
C  3. Extent#  2-chars
C  4. Dual purpose (4-chars):
C       For Type 50 & 51 extents: #blocks in extent (1 block=256 bytes)
C       For Type 52: #seconds in scan processed by FRNGE
C  5. Expt serial#  (3-chars)   (Haystack-assigned)
C  6. Processing date and time (10-chars) (6th char indicates COREL
C     version#)
C  7. Year of scan  (2-chars)
C  8. Scan ID (8-chars, in format DDD-HHMM of scheduled start time)
C  9. Source name  (8-chars)
C 10. Baseline  (2-chars)
C     FRNGE quality code (1-char) See below.
C 11. Reference tape#  (8-chars)
C 12. Remote tape#  (8-chars)
C 13. Frequency code  (1-char)
C     Mode  (1-char)
C     #Frequencies processed by FRNGE  (2-chars)
C 14. Correlation amplitude (F6.2 OR F6.1)  (units of 1.0E-04)
C 15. SNR (I4 format, clips at 9999)
C 16. Residual phase (Earth centered)  (deg F5.1)
C 17. Residual single-band delay  (usec F5.2)
C 18. Residual multi-band delay   (usec F7.4)
C 19. Residual delay rate         (psec/sec F7.2)
C 20. Tape quality codes  (6-chars)
C 21. "A-file#"  (5 chars)
C
C     Total of 131 characters in fields 1-21.
C     Normally, only these fields are printed out.
C     Columns beyond 131 contain additional information as follows:
C
C 22. UT Epoch (mmss) to which observables are referred
C 23. Total Earth-centered phase (degrees F5.1)
C 24. Total phase-delay rate (usec/sec F11.8)
C 25. Total multi-band delay (usec F13.6)
C 26. Total single-band delay minus total multiband delay (usec F5.3)
C 27. Multiband group-delay ambiguity (usec F4.3)
C 28. Type 51 extent from which data came, or 0 if data came from
C     multiple
C     Type 51 extents (this information starting in A1718).
C The following fields were added starting in A2923:
C 29. Phase of phase-cal for channel#1 on station 1 (0-360)
C 30. Phase of phase-cal for last channel on station 1
C 31. Phase of phase-cal for channel#1 on station 2 (0-360)
C 32. Phase of phase-cal for last channel on station 2
C
C MODIFIED NEW FORMAT (from A3459):
C
C Same as "new format" except for following fields--
C  2. Extent type (1-char) 0-ROOT 1-COREL 2-FRNGE (instead of 50,51,52)
C  3. Extent# (2-char)  shifted left 1-column
C  4. Dual purpose (4-chars) (shifted left 1-column):
C       For Type 50 & 51 extents: #blocks in extent (1 block=256 bytes)
C       For Type 52: #seconds in scan processed by FRNGE
C  5. Expt serial# expanded to allow 4-chars
C
C EXAMPLE (including title lines, column indicators, and key#'s):
C
C THE FOLLOWING LINES ARE > 72 CHARACTERS (WHICH IS A PAIN FOR
C DOCUMENTATION) SO ARE SPLIT AFTER CHARACTER 65
C
C *!NAME TYP X  LEN EX#   PROCDATE YR   RUN#   SOURCE   BSQ       T
C APES       FM#C   AMP  SNR RPHAS SBDLY   MBDLY  DRATE ESDESP AR
C *!           (sec)
C                       (deg) (usec)  (usec) (ps/s)
C <31800 2  2   75  113 82099?0133 82 083-1800 NRAO150  EF9 SAO-201
C 8 HS001481  XC08   7.68  21  70.9   .02  -.0193    .17 790306 A0
C *'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/
C ''''7''''/''''8''''/''''9''''/''''0''''/''''1''''/''''2''''/'''
C *F# 1   2  3    4  5       6      7    8        9     10     11
C     12      13     14   15   16    17     18       19   20
C
C Beyond character #150: Use LISTV to view as EDIT will only
C show 150 chars.
C
C  EPOC TPHAS   TOTDRATE    TOTMBDELAY  SBRES  AMB X51 -PCAL1- -PCAL2-
C  mmss (deg)  (usec/sec)     (usec)   (usec) (usec)    (deg)   (deg)
C  0055 354.4  -.34549878   6785.276752 -.095 .200   3 221 202  57  42
C '''/''''4''''/''''5''''/''''6''''/''''7''''/''''8''''/''''9''''/
C  22    23       24           25        26    27   28  29  30  31
C ''''0''''/
C  32
C
C FRNGE QUALITY CODE:
C
C Q= quality factor of the run,0= no fringes 3=poor,9=good
C             i.e. higher numbers indicate better quality
C Q quality factor starts at 9 and is reduced by:
C        1 point if RMS PH/SEG >11.46 and theoretical RMS < 5.73
C        2 points if RMS PH/SEG >22.92 and theoretical RMS <11.46
C the factor is further reduced by:
C        1 point if RMS AMP/SEG >20.00% and theoretical RMS <10.00%
C        2 points if RMS AMP/SEG >40.00% and theoretical RMS <20.00%
C the factor is futher reduced by:
C        1 point  if RMS PH/FRQ  > 11.46 and theoretical RMS < 5.73
C        2 points if RMS PH/FRQ  > 22.92 and theoretical RMS <11.46
C the factor is further reduced by:
C        1 point  if RMS AMP/FRQ  > 20.00% and theoretical RMS <10.00%
C        2 points if RMS AMP/FRQ  > 40.00% and theoretical RMS <20.00%
C any error condition sets the quality factor to  letter code in which
C                  A= freqency sequence cannot be handled by FRNGE
C                     requires an FFT of more than 256 points
C                  B= interpolation error check search range fringes
C                     may be on the edge of range
C                  C= epoch error condition all frequencies not at same
C                     epoch
C                  D= no phasecal or manually entered phasecal values
C                  E= single band delay residual too large CAUTION
C                     fringe amplitude may be more than 5% low
C                  F= no data found- dummy FRNGE output
C               Q=0= probability of false detection greater than 1.0E-9
C                     i.e. NO FRNGES
C               Q=1= one or more phase cal signals less than 1% or
C                     greater than 20% in voltage
C               Q=2= one or more channels have amplitude less than half
C                     the strength of the coherent average and SNR.GT.20
C
C TAPE QUALITY CODES:
C
C TAPEQ=6 character tape quality code
C         E (Char 1)=ref.tape error rate exponent i.e. 0=1.0E00 9=1.0E-9
C         S (Char 2)=ref.tape slip sync rate      i.e. 0=10%    9=1%
C         D (Char 3)=percentage data discarded    i.e. 0=10%    9=1%
C         E (Char 4)=rem.tape error rate exponent i.e. 0=1.0E00 9=1.0E-9
C         S (Char 5)=rem.tape slip sync rate      i.e. 0=10%    9=1%
C         P (Char 6)=fraction of data processed   i.e. 0=<5%    9=>95%
C                    that is, INTG.TIME/scheduled run duration
C
C
C "PSEUDO" A-FILES:
C
C     Several programs generate output in A-file like format as follows:
C
C CLOSR-Program which computes closure
C
C Field# 1-15 standard A-file
C Field# 16-19 same as A-file but with closure phase,singleband delay,
C              multiband  delay and rate.
C Field# 20. Closure triangle (A3),fixed "*",closure SNR (I2 format,
C              clips at 99)
C Field# 21 standard A-file
C Field# 22-28 blank
C
C FRNGX-Program which segments FRNGE output
C
C Field# 1-5 standard
C Field# 6. Processing date and time (6th char indicates mode)
C     ":"phases residual to COREL+FRNGE "."phases residual to COREL
C     "&"total observed phases "*"residual to COREL+FRNGE(offset coords)
C     "%"total earth centered
C Field# 7-9  standard
C Field# 10. Baseline (2-chars) FRNGE quality blank
C Field# 11. Decimal seconds of segment CENTER (F5.1)
C Field# 11A. Nominal start time or scan time (hrs 0=0,9=9,A=10 ; min)
C Field# 12. Frequency in MHZ  (F8.2) (mean of all frequencies
C            in sequence)
C Field# 13. Frequency code,blank,#frequencies
C Field# 14-15 standard (note amp. in field 14 is coherent and is NOT
C              noise corrected)
C Field# 16 residual phase (or difference phase if reference file
C           is used)
C Field# 17 incoherent amplitude (cumulative from first segment of
C           scan-NOISE-corrected)
C Field# 18 incoherent SNR       (cumulative from first segment of scan)
C Field# 19 reference file   amplitude or sum of phase cal phases-all
C           freqs or segment number (in mode 10)
C Field# 20 reference file name or Allan var.
C Field# 21 reference file name extent# or coherent integration sec
C Field# 22-28 blank
C
C FRNGX second line (when run in astrometric mode)
C
C Field# 1 standard
C Field# 2 "3"
C Field# 3 standard
C Field# 4 delay in ns
C Field# 5 delay error in ns
C Field# 6 delay rate in ps/s
C Field# 7 delay rate error in ps/s
C Field# 8 total phase deg
C Field# 9 ref. station elevation deg
C Field# 10 rem. station elevation deg
C Field# 11 Correlation coeff. for incoherent addition of frnge segments
C Field# 12 U of standard epoch(1950 or 2000) in millions of wavelengths
C Field# 13 V of standard epoch(1950 or 2000) in millions of wavelengths
C Field# 14 Probability of false detection
C
C VFIND-Program to search EC and A-files for various quantities
C
C Field #7.  The year is deleted, and in its place in I3 format is field
C            #29, the type 51 extent number from which this type 52 was
C            generated.
C
C CRITERIA FOR DATA EXPORT AND QF REPORTS
C
C Because there are processing duplicates in most experiments a choice
C has to be made on which one to accept.The method of duplicate deletion
C used is to first sort the A file of all scans so that all duplicates
C are adjacent and then RU,CLOSR,-64,File of all scans,File of last scan
C in each set of duplicates. With this scheme the criteria of acceptance
C depends on the order in which the duplicates are sorted.
C
C OLD METHOD: accepts most recent processing - used for astronomy
C
C SORT ORDER (MOST SIGNIFICANT FIRST ):
C  SCAN TIME(37:03,41:04),[SOURCE(46:08)],FREQ CODE(77:01),
C  BASELINE(55:02), PROC TIME(23:10)
C
C where [] indicates that this step is usually unnecessary i.e. there
C is normally only one source associated with one scan
C
C       () Afile column # start : # columns
C
C NEW METHOD: accepts quality factors 8 & 9  with highest SNR -used
C             for geodesy
C
C SORT ORDER (MOST SIGNIFICANT FIRST ):
C  SCAN TIME(37:03,41:04),[SOURCE(46:08)],FREQ CODE(77:01),
C  BASELINE(55:02), QFF(58:01),SNR(88:04),[PROC TIME(23:10)]
C
C where  QFF=2 for QF=8 and 9
C        QFF=1 for QF=7,6,5,4,3,2,1,0
C        QFF=0 for QF=letter codes  and fringe searches (NFREQ=1)
C        QFF is derived from Afile using EDIT commands
C
C
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      INTEGER   IERR, NPARM, ICHAR, JTRIM, I, ILINE, IROUND, JT
      REAL      TIM1, TIM2
      LOGICAL   T, APPEND
      INCLUDE 'AFILE.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T, BUFF1)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 35
      CALL GTPARM (PRGN, NPARM, RQUICK, XINFIL, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (48, 1, XINFIL, INFIL)
      CALL H2CHR (48, 1, XOUTFL, OUTFIL)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      IF ((OPCODE.NE.'SORT') .AND. (OPCODE.NE.'EDIT')) THEN
         WRITE (MSGTXT,1020) OPCODE
         JERR = 1
         GO TO 990
         END IF
      EXPNUM = IROUND(APARM(2))
      IF (EXPNUM.LE.0) EXPNUM = 0
C                                       Open text files
      LUN1 = 10
      LUN2 = 11
      APPEND = .FALSE.
      CALL ZTXOPN ('READ', LUN1, FIND1, INFIL, APPEND, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1010) JERR, INFIL
         GO TO 990
         END IF
      CALL ZTXOPN ('WRIT', LUN2, FIND2, OUTFIL, APPEND, JERR)
      IF (JERR.NE.0) THEN
         WRITE (MSGTXT,1010) JERR, OUTFIL
         GO TO 990
         END IF
C                                       Sort the data
      CALL DSORT (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Edit the data
      IF (OPCODE.EQ.'EDIT') THEN
         CALL DEDIT (JERR)
         IF (JERR.NE.0) GO TO 999
         END IF
C                                       Write the final file.
      JT = JTRIM (TIT1)
      CALL ZTXIO ('WRIT', LUN2, FIND2, TIT1(:JT), IERR)
      JT = JTRIM (TIT2)
      CALL ZTXIO ('WRIT', LUN2, FIND2, TIT2(:JT), IERR)
      ILINE = 0
      TIM1 = 0.0
      TIM2 = 0.0
      DO 800 I = 1, TOTLIN
         IF ((OPCODE.EQ.'EDIT') .AND. (AINDEX(I).EQ.0)) GO TO 800
         ICHAR = JTRIM (LINE(I))
         CALL ZTXIO ('WRIT', LUN2, FIND2, LINE(I)(:ICHAR), IERR)
         ILINE = ILINE + 1
         IF (ILINE.EQ.1) CALL TIMCRK (LINE(I)(37:), TIM1)
         IF (ILINE.GT.1) THEN
            CALL TIMCRK (LINE(I)(37:), TIM2)
            IF (TIM2.LT.TIM1) THEN
               WRITE (MSGTXT,1040) ILINE
               GO TO 990
               END IF
            TIM1 = TIM2
            END IF
 800     CONTINUE
      WRITE (MSGTXT,1030) ILINE
      CALL MSGWRT (6)
C
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  CALL ZTXCLS (LUN1, FIND1, IERR)
      CALL ZTXCLS (LUN2, FIND2, IERR)
      RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('AFILIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('AFILIN: ERROR',I3,' OPENING ',A)
 1020 FORMAT ('AFILIN: UNKNOWN OPCODE ',A4,' --- TRY AGAIN')
 1030 FORMAT ('Total number of type 52 lines in output A file = ',I7)
 1040 FORMAT ('AFILIN: line ',I6,' in output file out of time order')
      END
      SUBROUTINE DSORT (IERR)
C-----------------------------------------------------------------------
C  Routine to sort the data by time and baseline. will reject all
C  non type 52 records. It appears that type 52 summaries are the ones
C  we need for our purposes. So routine will first read up to 5000
C  type 52 records into memory, then set up a sorting index based
C  on time and baseline, then use the sorting index to write out the
C  records in the correct order.
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'AFILE.INC'
      CHARACTER TEMP*256, TLIN(NUMLIN)*256
      INTEGER   ILINE, KLINE, XBASE(NUMLIN), I, J, K, II, JTRIM, JT,
     *   INDX(NUMLIN), ITIME, NTIM(NUMLIN),
     *   NSTART(NUMLIN), NL, OLDI, IT1, IGRP, NSTOP
      REAL      XTIME(NUMLIN), T1
      LOGICAL   EOF, SORT
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                        Read a line and decode the
C                                        time into days
      NL = NUMLIN
      CALL FILL (NL, 0, NTIM)
      CALL FILL (NL, 0, NSTART)
      ILINE = 0
      KLINE = 0
      IERR = 0
      EOF = .FALSE.
      TIT1 = ' '
      TIT2 = ' '
 100  CALL ZTXIO ('READ', LUN1, FIND1, TEMP, IERR)
      IF (IERR.EQ.2) THEN
         EOF = .TRUE.
         IERR = 0
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         GO TO 990
         END IF
      IF (EOF) GO TO 200
      JT = JTRIM (TEMP)
      KLINE = KLINE + 1
C                                        Ignore comment lines,
C                                        except if 2nd character is !
      IF (TEMP(1:1).EQ.' ') GO TO 100
      IF ((TEMP(1:1).EQ.'*') .AND. (TEMP(2:2).NE.'!')) GO TO 100
      IF ((TEMP(2:2).EQ.'!') .AND. (TEMP(3:6).EQ.'NAME')) THEN
         IF (TIT1(1:1).EQ.'  ') TIT1 = TEMP
      ELSE IF ((TEMP(2:2).EQ.'!') .AND. (TEMP(3:6).NE.'NAME')) THEN
         IF (TIT2(1:1).EQ.' ') TIT2 = TEMP
         END IF
C                                        Is it a type 52?
      IF (TEMP(8:8).NE.'2') GO TO 100
C                                        OK we want this one.
      ILINE = ILINE + 1
      LINE(ILINE) = TEMP
C                                        Extract time and encode as
C                                        days
      CALL TIMCRK (TEMP(37:), TIMIND(ILINE))
C                                        Extract baseline names and
C                                        encode as A + 256 * B
      CALL BASCRK (TEMP(55:), BASIND(ILINE))
      GO TO 100
C                                        Now we sort by time
 200  DO 210 I = 1, ILINE
         XTIME(I) = TIMIND(I)
         XBASE(I) = BASIND(I)
         INDX(I) = I
         TLIN(I) = LINE(I)
 210     CONTINUE
C
      SORT = .FALSE.
      K = ILINE - 1
      DO 300 II = 1, ILINE
         DO 250 I = 1, K
            IF (TIMIND(I+1).LT.TIMIND(I)) THEN
               J = INDX(I)
               INDX(I) = INDX(I+1)
               INDX(I+1) = J
               T1 = TIMIND(I)
               TIMIND(I) = TIMIND(I+1)
               TIMIND(I+1) = T1
               END IF
 250        CONTINUE
 300     CONTINUE
C                                       reorder in memory
      ITIME = 0
      OLDI = 1
      TOTLIN = ILINE
      DO 450 I = 1, ILINE
         TIMIND(I) = XTIME(INDX(I))
         BASIND(I) = XBASE(INDX(I))
         LINE(I) = TLIN(INDX(I))
         IF (I.GT.1) THEN
            IF ((TIMIND(I).GT.TIMIND(I-1)).OR.(I.EQ.ILINE)) THEN
               ITIME = ITIME+ 1
               NTIM(ITIME) = I - OLDI
               IF (I.EQ.ILINE) NTIM(ITIME) = NTIM(ITIME) + 1
               NSTART(ITIME) = OLDI
               OLDI = I
               END IF
            END IF
 450     CONTINUE
C                                       Now sort by baseline order,
C                                       but must first gather groups
C                                       with the same time.
      DO 700 IGRP = 1, ITIME
         ILINE = NTIM(IGRP)
         K = ILINE + NSTART(IGRP) - 2
         NSTOP = NSTART(IGRP) + ILINE - 1
         DO 500 I = NSTART(IGRP), NSTOP
            XTIME(I) = TIMIND(I)
            XBASE(I) = BASIND(I)
            INDX(I) = I
            TLIN(I) = LINE(I)
 500        CONTINUE
C
         DO 550 II = NSTART(IGRP), NSTOP
            DO 520 I = NSTART(IGRP), K
               IF (BASIND(I+1).LT.BASIND(I)) THEN
                  J = INDX(I)
                  INDX(I) = INDX(I+1)
                  INDX(I+1) = J
                  IT1 = BASIND(I)
                  BASIND(I) = BASIND(I+1)
                  BASIND(I+1) = IT1
                  END IF
 520              CONTINUE
 550        CONTINUE
C                                       reorder in memory
         DO 610 I = NSTART(IGRP), NSTOP
            TIMIND(I) = XTIME(INDX(I))
            BASIND(I) = XBASE(INDX(I))
            LINE(I) = TLIN(INDX(I))
 610        CONTINUE
 700     CONTINUE
      WRITE (MSGTXT,1010) KLINE
      CALL MSGWRT (6)
      WRITE (MSGTXT,1020) TOTLIN
      CALL MSGWRT (6)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DSORT: ERROR ',I3,' READING INPUT A-FILE')
 1010 FORMAT ('Total number of lines in input A file = ',I7)
 1020 FORMAT ('Total number of type-52 lines in input A file = ',I7)
      END
      SUBROUTINE DEDIT (IERR)
C-----------------------------------------------------------------------
C  Routine to delete duplicate scans from the MkIII A-file. Upon
C  entering DEDIT the A-file will exist in memory, sorted by time
C  and baseline - so duplicate scans should lie together. This
C  routine will read all duplicate scans and delete those that are
C  unnecessary based on 4 criteria:
C    (1) take the one with the latest processing date
C    (2) take the one with the highest quality factor
C    (3) take the one with the highest signal-to-noise ratio
C    (4) take the one with the most data.
C  Also if EXPNUM > 0 will select scans based on experiment number
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INCLUDE 'AFILE.INC'
      CHARACTER TLIN(NUMLIN)*256, QCODE*1, EXPCOD*4,
     *   ALPNUM*36
      INTEGER   ILINE, KLINE, BASE, B1, I, FLINE, QINDEX(500),
     *   NLINE, NL, K, LACC, CORCOD(500), CMAX, YR(500), YMAX,
     *   SNR(500), DLEN(500), NUMEXP(1)
      REAL      TIME, T1
      DOUBLE PRECISION PTIME(500), TMAX
      LOGICAL   EOF
      INCLUDE 'INCS:DMSG.INC'
      DATA ALPNUM /'0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
C                                        Message to user
      IF (EXPNUM.GT.0) THEN
         WRITE (MSGTXT,1140) EXPNUM
         CALL MSGWRT (6)
         END IF
      IF (APARM(1).EQ.1.0) WRITE (MSGTXT,1100)
      IF (APARM(1).EQ.2.0) WRITE (MSGTXT,1110)
      IF (APARM(1).EQ.3.0) WRITE (MSGTXT,1120)
      IF (APARM(1).EQ.4.0) WRITE (MSGTXT,1130)
      CALL MSGWRT (6)
C                                        Read until time and/or
C                                        baseline changes.
      ILINE = 0
      KLINE = 0
      IERR = 0
      NL = NUMLIN
      CALL FILL (NL, 1, AINDEX)
      EOF = .FALSE.
      TIME = -1.0
      BASE = -1
C
 100  CONTINUE
      ILINE = ILINE + 1
      KLINE = KLINE + 1
      IF (ILINE.GT.TOTLIN) GO TO 200
      TLIN(ILINE) = LINE(ILINE)
      IF (KLINE.EQ.1) THEN
         FLINE = ILINE
         CALL TIMCRK (TLIN(ILINE)(37:), TIME)
         CALL BASCRK (TLIN(ILINE)(55:), BASE)
         END IF
      CALL TIMCRK (TLIN(ILINE)(37:), T1)
      CALL BASCRK (TLIN(ILINE)(55:), B1)
      IF ((T1.GT.TIME) .OR. (B1.NE.BASE)) GO TO 200
C                                        Special case for last line
      IF (ILINE.EQ.TOTLIN) THEN
C                                        Check experiment number
         IF (EXPNUM.GT.0) THEN
            EXPCOD = TLIN(ILINE)(18:)
            CALL GETI4 (1, EXPCOD, NUMEXP)
            IF (NUMEXP(1).NE.EXPNUM) AINDEX(ILINE) = 0
            END IF
         END IF
      GO TO 100
C                                        Do the editing.
 200  NLINE = KLINE - 1 + FLINE - 1
      KLINE = 0
      ILINE = ILINE - 1
C                                        Check for processing date,
C                                        choose latest
      IF (APARM(1).EQ.1.0) THEN
         K = 0
         LACC = FLINE
         TMAX = -1.0D10
         YMAX = -1
         CMAX = -1
         DO 250 I = FLINE, NLINE
            K = K + 1
            CALL PROCDT (TLIN(I)(23:), YR(K), PTIME(K), CORCOD(K))
            AINDEX(I) = 0
            IF (YR(K).EQ.YMAX) THEN
               IF (PTIME(K).GT.TMAX) THEN
                  TMAX = PTIME(K)
                  LACC = I
                  END IF
               END IF
C
            IF (YR(K).GT.YMAX) THEN
               YMAX = YR(K)
               TMAX = PTIME(K)
               LACC = I
               END IF
C
 250        CONTINUE
         AINDEX(LACC) = 1
         END IF
C                                        Check for quality factor,
C                                        choose highest
      IF (APARM(1).EQ.2.0) THEN
         K = 0
         LACC = FLINE
         YMAX = -1
         DO 300 I = FLINE, NLINE
            K = K + 1
            QCODE = TLIN(I)(57:57)
            QINDEX(K) = INDEX (ALPNUM,QCODE)
            IF (QINDEX(K).GT.10) QINDEX(K) = 1
            AINDEX(I) = 0
            IF (QINDEX(K).GT.YMAX) THEN
               YMAX = QINDEX(K)
               LACC = I
               END IF
 300        CONTINUE
         AINDEX(LACC) = 1
         END IF
C                                        Check for SNR
C                                        choose highest
      IF (APARM(1).EQ.3.0) THEN
         K = 0
         LACC = FLINE
         YMAX = -1
         DO 350 I = FLINE, NLINE
            K = K + 1
            CALL GETI4 (1, TLIN(I)(88:91), SNR(K))
            AINDEX(I) = 0
            IF (SNR(K).GT.YMAX) THEN
               YMAX = SNR(K)
               LACC = I
               END IF
 350        CONTINUE
         AINDEX(LACC) = 1
         END IF
C                                        Check for data length,
C                                        choose highest
      IF (APARM(1).EQ.4.0) THEN
         K = 0
         LACC = FLINE
         YMAX = -1
         DO 400 I = FLINE, NLINE
            K = K + 1
            CALL GETI4 (1, TLIN(I)(13:16), DLEN(K))
            AINDEX(I) = 0
            IF (DLEN(K).GT.YMAX) THEN
               YMAX = DLEN(K)
               LACC = I
               END IF
 400        CONTINUE
         AINDEX(LACC) = 1
         END IF
C                                        Check experiment number
      IF (EXPNUM.GT.0) THEN
         DO 710 I = FLINE, NLINE
            EXPCOD = TLIN(I)(18:)
            CALL GETI4 (1, EXPCOD, NUMEXP)
            IF (NUMEXP(1).NE.EXPNUM) AINDEX(I) = 0
 710        CONTINUE
         END IF
C                                       don't go back if last line has
C                                       been encountered
C
      IF (ILINE.LT.TOTLIN) GO TO 100
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Editing A-file based on last processing date')
 1110 FORMAT ('Editing A-file based on Quality Factor from FRNGE')
 1120 FORMAT ('Editing A-file based on Signal-to-Noise ratio')
 1130 FORMAT ('Editing A-file based on # seconds of data in scan')
 1140 FORMAT ('Selecting scans from experiment # ',I6)
      END
      SUBROUTINE TIMCRK (STRING, DAYS)
C-----------------------------------------------------------------------
C   Routine to crack the time from a character string
C   Input:
C     STRING       C*(*)       String containing time
C   Output:
C     DAYS         R           Time in days
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), CTEMP*20
      REAL      DAYS
      INTEGER   IVAL(3)
C-----------------------------------------------------------------------
      CTEMP = ' '
      CTEMP(1:3) = STRING(1:3)
      CTEMP(5:6) = STRING(5:6)
      CTEMP(8:9) = STRING(7:8)
      CALL GETI4 (3, CTEMP(:9), IVAL)
      DAYS = IVAL(1) + (IVAL(2)/24.) + (IVAL(3)/(24.*60.))
C
 999  RETURN
      END
      SUBROUTINE BASCRK (STRING, BASLN)
C-----------------------------------------------------------------------
C   Routine to crack the baseline from a character string, codify the
C   letters as numbers. BASLN = A*256 + B
C   Input:
C     STRING       C*(*)       String containing baseline
C   Output:
C     BASLN        I        Baseline number
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), A*1, B*1, ALPHA*26
      INTEGER   BASLN, I, J
      DATA ALPHA /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
C-----------------------------------------------------------------------
      A = STRING(1:1)
      B = STRING(2:2)
      I = INDEX(ALPHA,A)
      J = INDEX(ALPHA,B)
      BASLN = J + 256*I
      RETURN
      END
      SUBROUTINE PROCDT (STRING, YR, TIME, CORCOD)
C-----------------------------------------------------------------------
C   Routine to determine the processing date and COREL version.
C   Input:
C     STRING       C*(*)       String.
C   Output:
C     YR           I           Year of correlation
C     TIME         D           Time of processing (years)
C     CORCOD       I           Corel version (number = letter)
C-----------------------------------------------------------------------
      CHARACTER STRING*(*), C*1, CTEMP*20, ALPHA*26
      INTEGER   CORCOD, YR, IVAL(4)
      DOUBLE PRECISION TIME
      DATA ALPHA /'abcdefghijklmnopqrstuvwxyz'/
C-----------------------------------------------------------------------
      CTEMP = ' '
      CTEMP(1:2) = STRING(1:2)
      CTEMP(4:6) = STRING(3:5)
      C = STRING(6:6)
      IF (C.EQ.' ') CORCOD = 0
      IF (C.NE.' ') CORCOD = INDEX(ALPHA,C)
      CTEMP(8:9) = STRING(7:8)
      CTEMP(11:12) = STRING(9:10)
      CALL GETI4 (4, CTEMP(:12), IVAL)
      TIME = IVAL(2) + (IVAL(3)/24.) + (IVAL(4)/(24.*60.))
      YR = IVAL(1)
C
 999  RETURN
      END
      SUBROUTINE GETI4 (N, STRING, IVAL)
C-----------------------------------------------------------------------
C   Routine to extract N free-format integers from a string
C   Input:
C      N        I       Number integers desired
C      STRING   C*(*)   String containing numbers
C   Output
C      IVAL     I(*)    Integers - default 0
C-----------------------------------------------------------------------
      INTEGER   N, IVAL(*)
      CHARACTER STRING*(*)
C
      DOUBLE PRECISION X
      INTEGER   KBP, KBPLIM, I
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DERR.INC'
C-----------------------------------------------------------------------
      KBPLIM = LEN (STRING)
      KBP = 1
      CALL FILL (N, 0, IVAL)
C
      DO 20 I = 1,N
         ERRNUM = 0
         CALL GETNUM (STRING, KBPLIM, KBP, X)
         IF ((X.EQ.DBLANK) .OR. (ERRNUM.NE.0)) GO TO 999
         IF (X.GE.0.0D0) THEN
            IVAL(I) = X + 0.5D0
         ELSE
            IVAL(I) = X - 0.5D0
            END IF
 20      CONTINUE
C
 999  RETURN
      END
