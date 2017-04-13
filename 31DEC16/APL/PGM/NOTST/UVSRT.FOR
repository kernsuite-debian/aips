LOCAL INCLUDE 'UVSRT.INC'
      INTEGER   BUFSIZ
      PARAMETER (BUFSIZ = 1024 * 4096 * 5)
C
      INTEGER   CATBLK(256), SEQIN, SEQOUT, DISKIN, DISOUT, SORT1,
     *   SORT2, LENBU, NSORT, IN, OUT, KEY1, KEY2, NBUFF, BUFSZ,
     *   CATSAV(256)
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XSORT
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, SORT*4
      REAL   XSIN, XSOUT, XDISIN, XDISO, BADD(10), ROTATE, DEFER,
     *   BUFFER(BUFSIZ)
      COMMON /BUFRS/ BUFSZ, IN, OUT, BUFFER
      COMMON /UVINF/ CATSAV, NBUFF, SEQIN, SEQOUT, DISKIN, DISOUT,
     *   SORT1, SORT2, LENBU, NSORT, KEY1, KEY2
      COMMON /MAPHDR/ CATBLK
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, BADD, XSORT, ROTATE, DEFER
      COMMON /CHRCOM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, SORT
LOCAL END
      PROGRAM UVSRT
C-----------------------------------------------------------------------
C! Sorts uv data into a specified order.
C# UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2001, 2005, 2008, 2011, 2015
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
C   UVSRT sorts a uv data base into a specified order
C   Inputs:
C   AIPS adverb  Prg. name.          Description.
C   INNAME         NAMEIN        Name of the file to be sorted
C   INCLASS        CLASIN        Class of the file to be sorted.
C   INSEQ          SEQIN         Seq. number of file to be sorted.
C   INDISK         DISKIN        Disk number of file to be sorted.
C   OUTNAME        NAMOUT        Name of the output sorted file.
C   OUTCLASS       CLAOUT        Class of the output sorted file.
C   OUTSEQ         SEQOUT        Seq. number of output sorted file.
C   OUTDISK        DISKO         Disk number of the output file.
C   BADDISK        IBAD          Disk numbers of disks not to be used.
C   SORT           SORT          Two char. sort order, eg. 'XY'
C                                Second key varies faster
C   ROTATE         ROTATE        Angle by which data is to be rotated.
C   DEFER          DEFER         Defer output file creation
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   LUNM(10),IRET, IERR, LRECM
      INCLUDE 'UVSRT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA PRGM /'UVSRT '/
      DATA LUNM /16,17,18,19,20,21,22,23,24,25/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      IRET = 18
      CALL UVSRIN (PRGM, IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Read and partially sort data
C                                       to scratch file.
      IRET = 17
      CALL INSORT (IERR)
      IF (IERR.NE.0) GO TO 990
C                                       Merge data.
      IF (NVIS.GE.NSORT) THEN
         LRECM = LREC + 2
         IRET = 16
         CALL AMERGE (KEY1, KEY2, NVIS, LRECM, NSORT, NBUFF, LUNM,
     *      SCRVOL, SCRCNO, LENBU, BUFFER(IN), BUFFER(OUT), IERR)
         IF (IERR.NE.0) GO TO 990
         IRET = 15
         END IF
C                                       Write data back to catalogd
C                                       file
      CALL UVOUT (IRET)
C                                       Close down files
 990  CALL DIE (IRET, BUFFER)
C
 999  STOP
      END
      SUBROUTINE UVSRIN (PRGM, JERR)
C-----------------------------------------------------------------------
C   UVSRIN gets input parameters for UVSRT and creates an output file
C   if necessary.
C   Inputs: PRGM   C*6       Task name
C   Output: JERR   I         Error code: 0 => ok, else quit
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, XSORTC(11)*1, TYPES(6)*4, STAT*4, XSORT1*1,
     *   XSORT2*1, UTYPE*2
      HOLLERITH CATH(256)
      INTEGER  JERR, NOPT, OLDCNO, IROUND, NPARM, IERR, I, NTRY, IOFF,
     *   JOFF
      LOGICAL   T
      REAL      CATR(256)
      INCLUDE 'UVSRT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      EQUIVALENCE (CATR, CATH, CATBLK)
      DATA NTRY /6/
      DATA TYPES /'DEC ','DEC-','MM  ','GLON','ELON','Y   '/
      DATA XSORTC /'B','T','U','V','W','R','P','X','Y','Z','M'/
      DATA NOPT /11/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init IO characteristics
      CALL ZDCHIN (T)
      CALL VHDRIN
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
      BUFSZ = BUFSIZ * 2
C                                       Get input parameters.
      NPARM = 27
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAMEI, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (8)
         JERR = 8
         RQUICK = .FALSE.
         GO TO 999
         END IF
C                                       Crunch input parameters.
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISOUT = IROUND (XDISO)
C                                       Characters
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XSORT, SORT)
      DO 15 I = 1,10
         IBAD(I) = IROUND (BADD(I))
 15      CONTINUE
C                                        Default sort order is 'XY'
      IF (SORT.EQ.' ') SORT = 'XY  '
C                                       Decode sort order.
      XSORT1 = SORT(1:1)
      XSORT2 = SORT(2:2)
      SORT1 = 8
      SORT2 = 9
      DO 20 I = 1,NOPT
         IF (XSORT1.EQ.XSORTC(I)) SORT1 = I
         IF (XSORT2.EQ.XSORTC(I)) SORT2 = I
 20      CONTINUE
C                                       Restart AIPS.
      IF (RQUICK) CALL RELPOP (JERR, BUFFER, IERR)
C                                       Get CATBLK from old file.
      OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN, NLUSER
         CALL MSGWRT (8)
         JERR = 5
         GO TO 999
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (8)
         JERR = 5
         GO TO 999
         END IF
      NCFILE = 1
      FVOL(1) = DISKIN
      FCNO(1) = OLDCNO
      FRW(1) = 0
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, '      ', NAMOUT, CLAOUT,
     *   SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, SORT, 1, CATH(KITYP))
      CATBLK(KIIMS) = SEQOUT
      IOFF = 3
      DO 55 I = 1,NTRY
         CALL AXEFND (4, TYPES(I), KICTPN, CATH(KHCTP), JOFF, IERR)
         IF (IERR.EQ.0) IOFF = JOFF
         IERR = 0
 55      CONTINUE
      CATR(KRCRT+IOFF) = CATR(KRCRT+IOFF) + ROTATE
C                                       create output
      CALL COPY (256, CATBLK, CATSAV)
      IF (DEFER.LE.0) CALL CREOUT (JERR)
C                                       Put input file in READ
      UTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, FCNO(1), NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, 'READ', BUFFER, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVSRIN: ERROR',I7,' OBTAINING INPUT PARAMETERS')
 1015 FORMAT (2A1)
 1030 FORMAT ('ERROR',I7,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I7,' COPYING CATBLK ')
      END
      SUBROUTINE CREOUT (IERR)
C-----------------------------------------------------------------------
C   CREOUT creates the output file and copies tables and HI info
C   Output:
C      IRET     I   Error return
C-----------------------------------------------------------------------
      INTEGER   IERR
C
      INTEGER   LUN1, LUN2, NONOT
      CHARACTER UTYPE*2, NOTAB*2, HILINE*72
      HOLLERITH CATH(256)
      REAL      CATR(256)
      INCLUDE 'UVSRT.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (CATR, CATH, CATBLK)
      DATA LUN1, LUN2 /27,28/
      DATA NONOT, NOTAB /1, 'NX'/
C-----------------------------------------------------------------------
C                                       Create output file.
      CCNO = 1
      FRW(2) = 3
      CALL UVCREA (DISOUT, CCNO, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
C                                       Only overwrite input file
 60      ELSE IF ((CCNO.NE.FCNO(1)) .OR. (DISOUT.NE.DISKIN)) THEN
            WRITE (MSGTXT,1060)
            CALL MSGWRT (8)
            IERR = 8
            GO TO 999
C                                        Update existing CATBLK
 65      ELSE
            FRW(2) = 2
            CALL CATIO ('READ', DISOUT, CCNO, CATBLK, 'REST', BUFFER,
     *         IERR)
            IF (IERR.EQ.0) THEN
C                                       Put input file in READ
               UTYPE = 'UV'
               CALL CATDIR ('CSTA', DISKIN, FCNO(1), NAMEIN, CLAIN,
     *            SEQIN, UTYPE, NLUSER, 'CLRD', BUFFER, IERR)
               CALL CHR2H (2, SORT(1:2), 1, CATH(KITYP))
               CATR(KRCRT+4) = CATR(KRCRT+4) + ROTATE
               CALL CATIO ('WRIT', DISOUT, CCNO, CATBLK, 'WRIT', BUFFER,
     *            IERR)
               END IF
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1066) IERR
               CALL MSGWRT (5)
               END IF
            END IF
         END IF
 70   NCFILE = 2
      FVOL(2) = DISOUT
      FCNO(2) = CCNO
      FRW(2) = FRW(2) - 1
C                                       copy header keywords
      CALL KEYCOP (DISKIN, FCNO(1), DISOUT, CCNO, IERR)
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISOUT, FCNO(1), CCNO,
     *   CATBLK, BUFFER, BUFFER(257), IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1070) IERR
         CALL MSGWRT (5)
         GO TO 100
         END IF
C                                       New history
 80   CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2,
     *   BUFFER(257), IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, CATBLK(KIIMS), DISOUT, LUN2,
     *   BUFFER(257), IERR)
      IF (IERR.NE.0) GO TO 100
C                                       Add new sort order.
      WRITE (HILINE,1080) TSKNAM, SORT
      CALL HIADD (LUN2, HILINE, BUFFER(257), IERR)
C                                       Add rotation.
      IF (ROTATE.NE.0.0) THEN
         WRITE (HILINE,1081) TSKNAM, ROTATE
         CALL HIADD (LUN2, HILINE, BUFFER(257), IERR)
         END IF
C                                       Close history file.
 100  CALL HICLOS (LUN2, .TRUE., BUFFER(257), IERR)
      IERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       Copy tables
 120  CALL ALLTAB (NONOT, NOTAB, LUN1, LUN2, DISKIN, DISOUT, FCNO(1),
     *   CCNO, CATBLK, BUFFER, BUFFER(4097), IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1050 FORMAT ('ERROR',I7,' CREATING OUTPUT FILE')
 1060 FORMAT ('OUTPUT FILE ALREADY EXISTS, BUT IS NOT INPUT FILE')
 1066 FORMAT ('UVSRIN: ERROR',I7,' UPDATING NEW CATBLK')
 1070 FORMAT ('UVSRIN: ERROR',I7,' COPY/OPEN HISTORY FILE')
 1080 FORMAT (A6,' SORT = ','''',A2,''' / New sort order')
 1081 FORMAT (A6,' ROTATE = ',F10.2,' / Rotn. added')
      END
      SUBROUTINE INSORT (JERR)
C-----------------------------------------------------------------------
C   INSORT reads an input file, creates scratch files, and presorts
C   blocks of data onto a scratch file.
C   Output:
C      NBUFF    I    Number of AMERGE streams. in common
C      LENBU    I    No. vis records per read. in common
C      JERR     I    Return error code.: 0 = OK
C                      1 = buffers too small
C                      2 = can't make scratch files.
C                      3 = can't open input file.
C                      4 = sort failure.
C                      8 = I/O error
C-----------------------------------------------------------------------
      INTEGER   JERR
C
      INTEGER   ILRECO, INVSUM, MAXBUF, ILIM, BIND1, BIND2, IBSTRT,
     *   LRECO, IERR, LUN1, IND1, LUN2, IND2, INIO, IBUFSZ, NIOUT,
     *   NVSUM, IPTR2, I, IPTR1, J, IKEY, KKEY, KEY, KOUT, LENIO, BO,
     *   VO, ISIZE
      LOGICAL   T, F, EOI, NOROT
      REAL      TEMPU, TEMPV, CROT, SROT, XX, YY
      CHARACTER PHNAME*48
      INCLUDE 'UVSRT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA BO, VO, MAXBUF /1, 0, 4/
C-----------------------------------------------------------------------
C                                       Check if rotate wanted.
      NOROT = ROTATE.EQ.0.0
      CROT = COS (-ROTATE/57.29578)
      SROT = SIN (-ROTATE/57.29578)
C                                       Set keys.
      KEY1 = 1
      KEY2 = 2
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      LRECO = LREC + 2
C                                       Set length of buffers.
      NBUFF = MAXBUF
      LENBU = (BUFSZ - (4 * NBPS * (NBUFF+1))) /
     *   ((NBUFF+1) * LRECO * 2)
      ILIM = 204800 / NBUFF
      LENBU = MIN (ILIM, LENBU)
C                                       Set number pre sorted.
      NSORT = NBUFF * LENBU
      IN = 1
      OUT = IN + LRECO * LENBU + NBPS
C                                       Make sure scratch files are
C                                       two buffers larger than nec.
      CALL UVSIZE (LRECO, NVIS, ISIZE)
      CALL SCREAT (ISIZE, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         JERR = 2
         GO TO 999
         END IF
      CALL SCREAT (ISIZE, BUFFER, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR
         JERR = 2
         GO TO 999
         END IF
C                                       Open input file.
      LUN1 = 16
      CALL ZPHFIL ('UV', DISKIN, FCNO(1), 1, PHNAME, IERR)
      CALL ZOPEN (LUN1, IND1, DISKIN, PHNAME, T, F, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1060) IERR
         CALL MSGWRT (8)
         JERR = 3
         GO TO 999
         END IF
C                                       Open output file.
      LUN2 = 17
      CALL ZPHFIL ('SC', SCRVOL(1), SCRCNO(1), 1, PHNAME, IERR)
      CALL ZOPEN (LUN2, IND2, SCRVOL(1), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1070) IERR
         CALL MSGWRT (8)
         JERR = 3
         GO TO 995
         END IF
C                                       Initialize files, single buffer.
      INIO = LENBU
      IBUFSZ = LREC * 2 * LENBU + 2 * NBPS
      CALL UVINIT ('READ', LUN1, IND1, NVIS, VO, LREC, INIO, IBUFSZ,
     *   BUFFER(IN), BO, BIND1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR, 'READ'
         CALL MSGWRT (8)
         JERR = 8
         GO TO 990
         END IF
      IBUFSZ = LRECO * LENBU * 2  +  2 * NBPS
      NIOUT = LENBU
      CALL UVINIT ('WRIT', LUN2, IND2, NVIS, VO, LRECO, NIOUT, IBUFSZ,
     *   BUFFER(OUT), BO, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1080) IERR, 'WRIT'
         CALL MSGWRT (8)
         JERR = 8
         GO TO 990
         END IF
C                                       Prepare pointers and counters.
      EOI = F
C                                       Begin loop.
 200  CONTINUE
C                                       Read NBUFF buffers, add keys and
C                                       place in output buffer area.
C                                       Fill output buffer with
C                                       NBUFF input buffers.
         NVSUM = 0
         IPTR2 = OUT + BIND2 - 1
         DO 400 I = 1,NBUFF
            IF (EOI) GO TO 400
            CALL UVDISK ('READ', LUN1, IND1, BUFFER(IN), INIO, BIND1,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1200) 'READ',IERR
               CALL MSGWRT (8)
               JERR = 8
               GO TO 990
               END IF
C                                       Sum no. vis. records
            NVSUM = NVSUM + INIO
            IPTR1 = IN + BIND1 - 1
C                                       Check if all data read.
            IF (INIO.LE.0) EOI = T
            IF (EOI) GO TO 400
C                                       Loop thru input buffer adding
C                                       keys and writing to output
C                                       buffer.  Since AMERGE orders
C                                       by descending values of the keys
C                                       negate keys.
            DO 370 J = 1,INIO
               IKEY = 0
               KKEY = SORT1
C                                        Rotate data if necessary.
               IF (.NOT.NOROT) THEN
                  TEMPU = CROT * BUFFER(IPTR1+ILOCU) -
     *               SROT * BUFFER(IPTR1+ILOCV)
                  TEMPV = SROT * BUFFER(IPTR1+ILOCU) +
     *               CROT * BUFFER(IPTR1+ILOCV)
                  BUFFER(IPTR1+ILOCU) = TEMPU
                  BUFFER(IPTR1+ILOCV) = TEMPV
                  END IF
C                                       Loop thru keys.
               DO 350 KEY = 1,2
C                                       Select key.
                  GO TO (220, 230, 240, 250, 260, 270, 280, 290, 300,
     *               310, 320, 330), KKEY
C                                       Baseline number.
 220                 IF (ILOCB.GE.0) THEN
                        BUFFER(IPTR2+IKEY) = -BUFFER(IPTR1+ILOCB)
                     ELSE
                        BUFFER(IPTR2+IKEY) = -BUFFER(IPTR1+ILOCA2) -
     *                     4096.0 * BUFFER(IPTR1+ILOCA1)
                        END IF
                     GO TO 340
C                                       Time.
 230                 BUFFER(IPTR2+IKEY) = -BUFFER(IPTR1+ILOCT)
                     GO TO 340
C                                       U
 240                 BUFFER(IPTR2+IKEY) = -BUFFER(IPTR1+ILOCU)
                     GO TO 340
C                                       V
 250                 BUFFER(IPTR2+IKEY) = -BUFFER(IPTR1+ILOCV)
                     GO TO 340
C                                       W
 260                 BUFFER(IPTR2+IKEY) = -BUFFER(IPTR1+ILOCW)
                     GO TO 340
C                                       Baseline length.
 270                 XX = BUFFER(IPTR1+ILOCU)
                     YY = BUFFER(IPTR1+ILOCV)
                     BUFFER(IPTR2+IKEY) = -SQRT (XX*XX + YY*YY)
                     GO TO 340
C                                       Baseline PA
 280                 XX = BUFFER(IPTR1+ILOCU)
                     YY = BUFFER(IPTR1+ILOCV)
                     BUFFER(IPTR2+IKEY) = -ATAN2 (YY, XX+1.0E-20)
                     GO TO 340
C                                       Descending ABS(u)
 290                 BUFFER(IPTR2+IKEY) = ABS (BUFFER(IPTR1+ILOCU))
                     GO TO 340
C                                       Descending ABS(v)
 300                 BUFFER(IPTR2+IKEY) = ABS (BUFFER(IPTR1+ILOCV))
                     GO TO 340
C                                       Ascending ABS(u)
 310                 BUFFER(IPTR2+IKEY) = -ABS (BUFFER(IPTR1+ILOCU))
                     GO TO 340
C                                       Ascending ABS(v)
 320                 BUFFER(IPTR2+IKEY) = -ABS (BUFFER(IPTR1+ILOCV))
                     GO TO 340
C                                       Spare
 330                 GO TO 340
C                                       Get set for second key.
 340              IKEY = 1
                  KKEY = SORT2
 350              CONTINUE
C                                       Load rest of record into output
C                                       buffer.
               CALL RCOPY (LREC, BUFFER(IPTR1), BUFFER(IPTR2+2))
C                                       Update pointers.
               IPTR1 = IPTR1 + LREC
               IPTR2 = IPTR2 + LRECO
 370           CONTINUE
 400        CONTINUE
         IF (NVSUM.LE.0) GO TO 520
C                                       Output buffer now loaded ready
C                                       to sort.
         KOUT = OUT + BIND2 - 1
         ILRECO = LRECO
         INVSUM = NVSUM
         CALL ICSORT (BUFFER(KOUT), ILRECO, INVSUM, KEY1, KEY2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1400) IERR
            CALL MSGWRT (8)
            JERR = 4
            GO TO 990
            END IF
C                                       Dump output buffer to disk
C                                       Trick UVDISK by moving buffer
C                                       pointer adjusted to take care
C                                       of unwritten data.
         IBSTRT = BIND2 - 1
         DO 500 I = 1,NBUFF
            IF (NVSUM.LE.0) GO TO 510
            NIOUT = MIN (LENBU, NVSUM)
            NVSUM = NVSUM - NIOUT
            LENIO = (I-1) * LENBU * LRECO  +  IBSTRT
            KOUT = OUT + LENIO - MOD (LENIO, NBPS/2)
            CALL UVDISK ('FLSH', LUN2, IND2, BUFFER(KOUT), NIOUT, BIND2,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1200) 'FLSH', IERR
               CALL MSGWRT (8)
               JERR = 8
               GO TO 990
               END IF
            IF (NIOUT.LE.0) GO TO 510
 500        CONTINUE
C                                        Copy any unwritten data
C                                        to start of buffer.
 510     IF (BIND2.GT.1) CALL RCOPY (BIND2, BUFFER(KOUT), BUFFER(OUT))
C                                       If not finished loop back
         IF (.NOT.EOI) GO TO 200
C                                       Empty buffer.
 520  NIOUT = 0
      IF (BIND2.GT.1) THEN
         CALL UVDISK ('FLSH', LUN2, IND2, BUFFER(KOUT), NIOUT, BIND2,
     *      IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1200) 'FLSH', IERR
            CALL MSGWRT (8)
            JERR = 8
            END IF
         END IF
C                                       Switch IN and OUT.
 550  I = IN
      IN = OUT
      OUT = I
C                                       Close files.
 990  CALL ZCLOSE (LUN2, IND2, IERR)
 995  CALL ZCLOSE (LUN1, IND1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('BUFFER SIZE',F8.0,' TOO SMALL FOR LREC=',I6)
 1030 FORMAT ('ERROR',I3,' CREATING SCRATCH FILE')
 1060 FORMAT ('INSORT: OPEN ERROR',I3,' ON INPUT FILE')
 1070 FORMAT ('INSORT: OPEN ERROR',I3,' ON SCRATCH FILE')
 1080 FORMAT ('INSORT: INIT ERROR',I3,' ON ',A4)
 1200 FORMAT ('INSORT:',A4,' ERROR',I3)
 1400 FORMAT ('INSORT: ERROR',I3,' DURING INCORE SORT')
      END
      SUBROUTINE UVOUT (JERR)
C-----------------------------------------------------------------------
C   UVOUT copies the data on a scratch file to a catalogd file,
C   removing the keys.
C   Inputs (from common):
C      NAMOUT             C*12 Output file name.
C      CLAOUT             C*6  Output file class.
C      DISOUT             I    Output file disk number.
C      SEQOUT             I    Output file sequence number.
C      SCRVOL(1)          I  Scratch file disk number.
C      SCRCNO(1)          I    Scratch file catalog number
C      LENBU              I    Buffer length in records.
C      LREC               I    Output record length in I   words.
C   Output:
C      JERR               I    Return error code: 0 = OK
C                              8 = I/O error.
C-----------------------------------------------------------------------
      INTEGER   LUN1, LUN2, IND1, IND2, BIND1, BIND2, JERR, IERR,
     *   IBUFSZ, LRECIN, NIOUT, IPTR2, INIO, IPTR1, I, BO, VO, NF, RW
      LOGICAL   T
      CHARACTER PHNAME*48
      INCLUDE 'UVSRT.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA T /.TRUE./
      DATA LUN1, LUN2 /16, 17/
      DATA BO, VO /1, 0/
C-----------------------------------------------------------------------
C                                       Kill second SC file
      RW = 2
      NF = 1
      CALL MAPCLR (NF, SCRVOL(2), SCRCNO(2), RW, BUFFER)
C                                       Deferred creation
      IF (DEFER.GT.0.0) THEN
         CALL COPY (256, CATSAV, CATBLK)
         CALL CREOUT (JERR)
         IF (JERR.NE.0) GO TO 999
         END IF
      JERR = 8
C                                       Open files
      CALL ZPHFIL ('UV', DISOUT, FCNO(2), 1, PHNAME, IERR)
      CALL ZOPEN (LUN2, IND2, DISOUT, PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, 'WRIT'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      CALL ZPHFIL ('SC', SCRVOL(1), SCRCNO(1), 1, PHNAME, IERR)
      CALL ZOPEN (LUN1, IND1, SCRVOL(1), PHNAME, T, T, T, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR,' READ'
         CALL MSGWRT (8)
         GO TO 995
         END IF
C                                       Init files, output first.
      IBUFSZ = BUFSZ / 2
      IN = 1
      OUT = IBUFSZ / 2 + 1
      LRECIN = LREC + 2
      NIOUT = LENBU
      CALL UVINIT ('WRIT', LUN2, IND2, NVIS, VO, LREC, NIOUT, IBUFSZ,
     *   BUFFER(OUT), BO, BIND2, IERR)
      IPTR2 = BIND2 + (OUT-1)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, 'WRIT'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Init input file.
      INIO = LENBU
      CALL UVINIT ('READ', LUN1, IND1, NVIS, VO, LRECIN, INIO, IBUFSZ,
     *   BUFFER(IN), BO, BIND1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1020) IERR, 'READ'
         CALL MSGWRT (8)
         GO TO 990
         END IF
C                                       Begin loop
 100  CONTINUE
         CALL UVDISK ('READ', LUN1, IND1, BUFFER(IN), INIO, BIND1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) 'READ',IERR
            CALL MSGWRT (8)
            GO TO 990
            END IF
         IPTR1 = BIND1 + 1 + IN
C                                       Copy to output.
         IF (INIO.GT.0) THEN
            DO 150 I = 1,INIO
               CALL RCOPY (LREC, BUFFER(IPTR1), BUFFER(IPTR2))
C                                       Update pointers
               IPTR1 = IPTR1 + LRECIN
               IPTR2 = IPTR2 + LREC
 150           CONTINUE
            NIOUT = INIO
            CALL UVDISK ('WRIT', LUN2, IND2, BUFFER(OUT), NIOUT, BIND2,
     *         IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1100) 'WRIT', IERR
               CALL MSGWRT (8)
               GO TO 990
               END IF
            IPTR2 = BIND2 + (OUT-1)
C                                       Loop back for next buffer.
            GO TO 100
            END IF
 200     CONTINUE
C                                       Finish write.
      NIOUT = 0
      CALL UVDISK ('FLSH', LUN2, IND2, BUFFER(OUT), NIOUT, BIND2, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) 'FLSH', IERR
         CALL MSGWRT (8)
         END IF
C                                       Close files.
 990  IF (IERR.EQ.0) JERR = 0
      CALL ZCLOSE (LUN1, IND1, IERR)
 995  CALL ZCLOSE (LUN2, IND2, IERR)
C                                       Kill first SC file
      RW = 2
      NF = 1
      CALL MAPCLR (NF, SCRVOL(1), SCRCNO(1), RW, BUFFER)
      NSCR = 0
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVOUT: ERROR',I3,' OPEN FOR ',A4)
 1020 FORMAT ('UVOUT: ERROR',I3,' INIT FOR ',A4)
 1100 FORMAT ('UVOUT:',A4,' ERROR ',I3)
      END
