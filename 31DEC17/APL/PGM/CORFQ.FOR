      PROGRAM CORFQ
C-----------------------------------------------------------------------
C! CORFQ corrects uvw for incorrect frequency.
C# UV VLA
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 2000, 2002, 2009, 2014
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
C   CORFQ is an AIPS-System task to correct uvw for incorrect frequency
C   Inputs:
C     AIPS Adverb   Prg. Name          Description
C     NAMEIN         NAME          File name of data base to be
C                                   corrected.
C     CLASSIN        CLASS         Input file class.
C     SEQIN          SEQ           Input file sequence number.
C     DISKIN         DISK          Disk volumn on which file resides.
C     FACTOR         FACTOR        Correction for FREQ-uvw
C                                  0.0 => Corrects 2cm problem.
C   Programmer: W. D. Cotton, Sept. 1981.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:ZPBUFSZ.INC'
      CHARACTER PRGM*6, NAME*12, CLASS*6, FILE*48, UTYPE*2
      HOLLERITH XNAME(3), XCLASS(2)
      LOGICAL   RQUICK, T, F, UPDATE, DOSA, DOHEAD
      INTEGER   DISK, SEQ, LUN, FIND, BIND, IROUND,
     *   IPT2, BUFSZ, LUNH, LENBU, LUN2, FIND2, CNO2, USERID, BIND2,
     *   IERR, JERR, IRET, LUNA,
     *   NPARM, IVER, NIO, IPOINT, NKEY, NREC, NCOL, DATP(2,128),
     *   J, LOCS(1), KEYTYP(1)
      INTEGER   VO, BO, XCOUNT, ABUFF(512)
      REAL      XSEQIN, XVIN, BANDW, BUFF(UVBFSS), BUFF2(UVBFSS), XFAC
      DOUBLE PRECISION    FACTOR, VALUES(1)
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:PUVD.INC'
      COMMON /INPARM/ XNAME, XCLASS, XSEQIN, XVIN, XFAC
      EQUIVALENCE (ABUFF, BUFF), (DATP, BUFF2)
      DATA LUN, LUN2, LUNH, LUNA /16, 17, 27, 28/
      DATA T, F /.TRUE.,.FALSE./
      DATA VO, BO, LENBU /0, 1, 16/
      DATA PRGM /'CORFQ '/
C-----------------------------------------------------------------------
C                                       Init I/O
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
C                                       Get input parameters.
      IRET = 0
      NPARM = 8
      CALL GTPARM (PRGM, NPARM, RQUICK, XNAME, BUFF, IERR)
      IF (IERR.EQ.0) GO TO 10
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
            IRET = 8
C                                       Restart AIPS.
 10   IF (RQUICK) CALL RELPOP (IRET, BUFF, IERR)
      IF (IRET.NE.0) GO TO 990
      IRET = 8
C                                       Decode input.
      CALL H2CHR (12, 1, XNAME, NAME)
      CALL H2CHR (6, 1, XCLASS, CLASS)
      USERID = NLUSER
      SEQ = IROUND (XSEQIN)
      DISK = IROUND (XVIN)
      FACTOR = ABS (XFAC)
      DOHEAD = XFAC.GE.0.0
C                                       Open file and get CATBLK.
      UPDATE = F
      UTYPE = 'UV'
      CALL MAPOPN ('HDWR', DISK, NAME, CLASS, SEQ, UTYPE, USERID, LUN2,
     *   FIND2, CNO2, CATBLK, BUFF2, IERR)
      IF (IERR.LE.1) GO TO 30
         WRITE (MSGTXT,1010) IERR
         CALL MSGWRT (8)
         GO TO 990
C                                       Get info from CATBLK.
 30   CALL UVPGET (IRET)
      IF (IRET.NE.0) GO TO 910
C                                       Determine FACTOR if default.
      BANDW = CATR(KRCIC+JLOCF)
      IF (BANDW.LE.0.0) BANDW = 50.0E6
      IF (FACTOR.LE.0.0) FACTOR = (FREQ - BANDW) / FREQ
      IF (DOHEAD) CATD(KDCRV+JLOCF) = FREQ * FACTOR
C                                       Initialize writing VIS. file.
      BUFSZ = UVBFSS * 2
      CALL UVINIT ('WRIT', LUN2, FIND2, NVIS, VO, LREC, LENBU, BUFSZ,
     *   BUFF2, BO, BIND2, IERR)
      IPT2 = BIND2
      IF (IERR.EQ.0) GO TO 35
         WRITE (MSGTXT,1030) IERR
         CALL MSGWRT (8)
         GO TO 910
 35   XCOUNT = 0
C                                       Open for Read.
      IVER = 1
      CALL ZPHFIL ('UV', DISK, CNO2, IVER, FILE, IERR)
      CALL ZOPEN (LUN, FIND, DISK, FILE, T, F, F, IERR)
      IF (IERR.GT.0) GO TO 910
      CALL UVINIT ('READ', LUN, FIND, NVIS, VO, LREC,LENBU, BUFSZ, BUFF,
     *   BO, BIND, IERR)
      IF (IERR.NE.0) GO TO 900
C                                       Start looping thru data.
 50   CONTINUE
C                                       Read buffer.
         XCOUNT = XCOUNT + 1
         CALL UVDISK ('READ', LUN, FIND, BUFF, NIO, BIND, IERR)
         IF (IERR.EQ.0) GO TO 60
            WRITE (MSGTXT,1050) IERR, XCOUNT
            CALL MSGWRT (8)
            GO TO 900
 60      IPOINT = BIND
         IF (NIO.LE.0) GO TO 90
C                                       Loop thru buffer.
            DO 70 J = 1,NIO
C                                       Correct uvw
               BUFF(IPOINT+ILOCU) = BUFF(IPOINT+ILOCU) * FACTOR
               BUFF(IPOINT+ILOCV) = BUFF(IPOINT+ILOCV) * FACTOR
               BUFF(IPOINT+ILOCW) = BUFF(IPOINT+ILOCW) * FACTOR
C                                       Copy to output buffer.
               CALL RCOPY (LREC, BUFF(IPOINT), BUFF2(IPT2))
C                                       Update IPOINT
               IPOINT = IPOINT + LREC
               IPT2 = IPT2 + LREC
 70            CONTINUE
C                                       Write output.
            CALL UVDISK ('WRIT', LUN2, FIND2, BUFF2, NIO, BIND2, IERR)
            IF (IERR.EQ.0) GO TO 80
               WRITE (MSGTXT,1070) IERR, XCOUNT
               CALL MSGWRT (8)
               GO TO 900
 80         IPT2 = BIND2
            GO TO 50
 90      CONTINUE
C                                       Flush output.
      NIO = 0
      CALL UVDISK ('FLSH', LUN2, FIND2, BUFF2, NIO, BIND2, IERR)
      IF (IERR.EQ.0) GO TO 100
         WRITE (MSGTXT,1070) IERR, XCOUNT
         CALL MSGWRT (8)
         GO TO 900
C                                       History
 100  CALL HIINIT (2)
      CALL HIOPEN (LUNH, DISK, CNO2, BUFF2, IERR)
      WRITE (MSGTXT,1100) PRGM, FACTOR
      CALL HIADD (LUNH, MSGTXT, BUFF2, IERR)
      CALL HICLOS (LUNH, T, BUFF2, IERR)
      IRET = 0
C                                       Change AN header.
 900  UPDATE = T
      IF (DOHEAD) THEN
         NKEY = 0
         NCOL = MAXANC
         CALL TABINI ('WRIT', 'AN', DISK, CNO2, 1, CATBLK, LUNA, NKEY,
     *      NREC, NCOL, DATP, ABUFF, JERR)
         IF (JERR.NE.0) GO TO 950
C                                       Update frequency
         MSGSUP = 32000
         DOSA = T
         CALL TABKEY ('READ', 'SAFREQ  ', 1, ABUFF, LOCS, VALUES,
     *      KEYTYP, IERR)
C                                       Check that 'FREQ' not used
C                                       instead of 'SAFREQ'
         IF (IERR.GT.0) THEN
            IERR = 0
            MSGSUP = 0
            CALL TABKEY ('READ', 'FREQ    ', 1, ABUFF, LOCS, VALUES,
     *         KEYTYP, IERR)
            IF (IERR.GT.0) THEN
               WRITE (MSGTXT,1200)
               GO TO 990
               END IF
            DOSA = F
         END IF
         VALUES(1) = VALUES(1) * FACTOR
         IF (DOSA) THEN
            CALL TABKEY ('WRIT', 'SAFREQ  ', 1, ABUFF, LOCS, VALUES,
     *         KEYTYP, IERR)
         ELSE
            CALL TABKEY ('WRIT', 'FREQ    ', 1, ABUFF, LOCS, VALUES,
     *         KEYTYP, IERR)
            END IF
         IF (JERR.LE.0) CALL TABIO ('CLOS', 1, XCOUNT, DATP, ABUFF,
     *      IERR)
         END IF
C                                       Close file.
 950  CALL ZCLOSE (LUN, FIND, JERR)
      IF (IERR.EQ.0) GO TO 910
         WRITE (MSGTXT,1900) JERR
         CALL MSGWRT (6)
C
 910  CALL MAPCLS ('WRIT', DISK, CNO2, LUN2, FIND2, CATBLK, UPDATE,
     *   BUFF, JERR)
C
 990  IRET = IERR
      CALL DIETSK (IRET, RQUICK, BUFF)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('ERROR:',I3,'GETTING INPUT PARAMETERS')
 1010 FORMAT ('ERROR:',I3,' OPENING UV FILE')
 1030 FORMAT ('ERROR:',I3,' INITILIZING UV FILE')
 1050 FORMAT ('ERROR:',I3,' READING VIS BLOCK ',I10)
 1070 FORMAT ('ERROR:',I3,' WRITING VIS BLOCK ',I10)
 1100 FORMAT (A5,1X,' Factor =',F10.5,' / freq-uvw correction')
 1200 FORMAT ('ERROR:',I3,' FINDING (SA)FREQ IN AN FILE, CHECK IT')
 1900 FORMAT ('ERROR:',I3,' CLOSING UV FILE ')
      END
