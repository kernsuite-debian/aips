LOCAL INCLUDE 'UVCMP.INC'
C                                       Local include for UVCMP
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XSOUT, XDISO, XCENT,
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ, KLOCWT,
     *   NRPRIN, LRECIN
      LOGICAL   DOCOMP
      DOUBLE PRECISION UVSCAL
      CHARACTER NAMEIN*12, CLAIN*6, OPTYPE*4, NAMOUT*12, CLAOUT*6,
     *   HISCRD(10)*64
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XCENT
      COMMON /LOCPRM/ UVSCAL, SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS,
     *   KLOCWT, NRPRIN, LRECIN, DOCOMP, DIFPIX
      COMMON /CHARPM/ NAMEIN, CLAIN, OPTYPE, NAMOUT, CLAOUT, HISCRD
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
LOCAL END
      PROGRAM UVCMP
C-----------------------------------------------------------------------
C! Compresses or uncompresses uv data
C# Utility UV UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000, 2008, 2014
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
C   UVCMP converts between normal data and compressed format.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input UV data.
C      OPTYPE         OPTYPE        'COMP' or 'UNCO'
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'UVCMP.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVCMP '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file.
      CALL UVCMIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Compress data.
      CALL UVCPAK (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVCHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVCMIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVCMIN gets input parameters for UVCMP and creates an output file.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, BLANK*6, TYPTMP*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, WTOFF, SCLOFF, INCX
      INCLUDE 'UVCMP.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /'      '/
C-----------------------------------------------------------------------
      JBUFSZ = UVBFSS * 2
      NUMHIS = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 15
      CALL SETUP (PRGN, NPARM, XNAMEI, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      TYPTMP = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, TYPTMP,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'READ', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Intellegent default OPTYPE
      IF (CATBLK(KINAX).GT.1) THEN
         OPTYPE = 'COMP'
      ELSE
         OPTYPE = 'UNCO'
         END IF
      DOCOMP = OPTYPE.EQ.'COMP'
      IF (JLOCF.LT.0) XCENT = -1.0
      IF (XCENT.LE.0.0) THEN
         UVSCAL = 1.0D0
         DIFPIX = 0.0
      ELSE
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = INCX - CATR(KRCRP+JLOCF)
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
         UVSCAL = CATD(KDCRV+JLOCF) / FREQ
         END IF
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Munge header
      NRPRIN = NRPARM
      LRECIN = LREC
      IF (DOCOMP) THEN
C                                       Already packed?
         IF (CATBLK(KINAX).EQ.1) THEN
            JERR = 9
            MSGTXT = 'ERROR: UV DATA IS ALREADY COMPRESSED'
            GO TO 990
            END IF
C                                       Compress - make sure that the
C                                       random parameter list contains a
C                                       WEIGHT parameter immediately
C                                       followed by a SCALE parameter:
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), WTOFF,
     *      IERR)
         IF (IERR.NE.0) THEN
            WTOFF = -1
            IERR = 0
            END IF
C                                       WTOFF is now the offset of the
C                                       WEIGHT parameter if it already
C                                       exists and -1 otherwise.
         CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP), SCLOFF,
     *      IERR)
         IF (IERR.NE.0) THEN
            SCLOFF = -1
            IERR = 0
            END IF
C                                       SCLOFF is now the offset of the
C                                       SCALE parameter if it already
C                                       exists an -1 otherwise.
         IF ((WTOFF.GT.-1) .AND. (WTOFF.LT.NRPARM-1) .AND.
     *      (SCLOFF.NE.WTOFF+1)) THEN
            CALL DELRPM (WTOFF)
            WTOFF = -1
            END IF
         IF (WTOFF.EQ.-1) THEN
            CALL APPRPM ('WEIGHT  ')
            CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP),
     *         WTOFF, IERR)
            END IF
         IF ((SCLOFF.NE.-1).AND.(SCLOFF.NE.WTOFF+1)) THEN
            CALL DELRPM (SCLOFF)
            SCLOFF = -1
            END IF
         IF (SCLOFF.EQ.-1) THEN
            CALL APPRPM ('SCALE   ')
            CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP),
     *         SCLOFF, IERR)
            END IF
         KLOCWT = WTOFF
         CATBLK(KINAX) = 1
      ELSE
C                                       Decompress
C                                       Already unpacked?
         IF (CATBLK(KINAX).GT.1) THEN
            JERR = 9
            MSGTXT = 'ERROR: UV DATA IS ALREADY DECOMPRESSED'
            GO TO 990
            END IF
C                                       Look for Weight and scale
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), WTOFF,
     *      IERR)
         KLOCWT = WTOFF
         CALL AXEFND (8, 'SCALE   ', CATBLK(KIPCN), CATH(KHPTP), SCLOFF,
     *      IERR)
C                                       Set Complex axis dim=3
         CATBLK(KINAX) = 3
C                                       Remove weight and scale rand
C                                       parms.
         CALL DELRPM (SCLOFF)
         CALL DELRPM (WTOFF)
         END IF
C                                       Create output file.
      CCNO = 1
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1050) IERR
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = 2
C                                       Get new uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVCMIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
      END
      SUBROUTINE UVCPAK (IRET)
C-----------------------------------------------------------------------
C   UVCPAK compresses the uv data.
C   Output:
C      IRET   I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48, IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, ILENBU, J,
     *   KBIND, NIOUT, NIOLIM, IBIND, I, BO, VO, NUMVIS, NCORR, XCOUNT
      LOGICAL   T, F
      INCLUDE 'UVCMP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUNI, LUNO /16, 17/
      DATA VO, BO /0, 1/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Number of correlators
      IF (DOCOMP) THEN
         NCORR = LREC - NRPARM
      ELSE
         NCORR = LRECIN - NRPRIN
         END IF
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE-1), 1, IFILE, IRET)
      CALL ZOPEN (LUNI, INDI, DISKIN, IFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1000) IRET
         GO TO 990
         END IF
C                                       Open vis file for write
      CALL ZPHFIL ('UV', DISKO, CCNO, 1, OFILE, IRET)
      CALL ZOPEN (LUNO, INDO, DISKO, OFILE, T, F, F, IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Init vis file for write
      ILENBU = 0
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LREC, ILENBU, JBUFSZ,
     *   BUFF2, BO, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1020) IRET
         GO TO 990
         END IF
      IPTRO = KBIND
      NIOUT = 0
      NIOLIM = ILENBU
C                                       Init vis file for read.
      ILENBU = 0
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECIN, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
      NUMVIS = 0
      XCOUNT = 0
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Out of data?
         IF (INIO.LE.0) GO TO 200
C                                       Loop over buffer
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS + 1
            BUFF1(IPTRI+ILOCU) = BUFF1(IPTRI+ILOCU) * UVSCAL
            BUFF1(IPTRI+ILOCV) = BUFF1(IPTRI+ILOCV) * UVSCAL
            BUFF1(IPTRI+ILOCW) = BUFF1(IPTRI+ILOCW) * UVSCAL
C                                       Copy random parms
            DO 120 J = 1,NRPRIN
               BUFF2(IPTRO+J-1) = BUFF1(IPTRI+J-1)
 120           CONTINUE
C                                       Compress
            IF (DOCOMP) THEN
               CALL ZUVPAK (NCORR, BUFF1(IPTRI+NRPRIN),
     *            BUFF2(IPTRO+KLOCWT), BUFF2(IPTRO+NRPARM))
C                                       Decompress
            ELSE
               CALL ZUVXPN (NCORR, BUFF1(IPTRI+NRPRIN),
     *            BUFF1(IPTRI+KLOCWT), BUFF2(IPTRO+NRPARM))
               END IF
C                                       Update pointers, counters
               XCOUNT = XCOUNT + 1
               IPTRO = IPTRO + LREC
               NIOUT = NIOUT + 1
               IPTRI = IPTRI + LRECIN
C                                       Write vis record.
         IF (NIOUT.LT.NIOLIM) GO TO 190
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
 190        CONTINUE
C                                       Read next buffer.
         GO TO 100
C                                       How many to history.
 200  NUMHIS = 1
      IF (DOCOMP) THEN
         WRITE (HISCRD(1),1200) XCOUNT
      ELSE
         WRITE (HISCRD(1),1201) XCOUNT
         END IF
C                                       Finish write
      NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                      Put vis. count in CATBLK
      CATBLK(KIGCN) = XCOUNT
C                                       Close files
      CALL ZCLOSE (LUNI, INDI, IRET)
      CALL ZCLOSE (LUNO, INDO, IRET)
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVCPAK: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('UVCPAK: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('UVCPAK: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('UVCPAK: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('UVCPAK: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('UVCPAK: ERROR',I3,' WRITING VIS FILE')
 1200 FORMAT ('Wrote ',I9,' compressed UV data records')
 1201 FORMAT ('Wrote ',I9,' uncompressed UV data records')
      END
      SUBROUTINE UVCHIS
C-----------------------------------------------------------------------
C   UVCHIS copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'UVCMP.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /0, '  '/
C-----------------------------------------------------------------------
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE-1),
     *   FCNO(NCFILE), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                       OPTYPE
      WRITE (MSGTXT,1001) TSKNAM, OPTYPE
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any history from common.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1010) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200   CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO,
     *   FCNO(1), FCNO(2), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, FCNO(2), DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'UVCHIS: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVCHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1001 FORMAT (A6,' OPTYPE =''',A,'''')
 1010 FORMAT (A6,' /')
 1200 FORMAT ('UVCHIS: ERROR COPYING TABLES')
      END
      SUBROUTINE DELRPM (IOFF)
C-----------------------------------------------------------------------
C   Delete the random parameter at offset IOFF from the random parameter
C   list (0 <= IOFF < CATBLK(KIPCN)). If the indicated parameter is at
C   the end of the random parameter list then the number of random
C   parameters is decreased by one, otherwise the name of the random
C   parameter is set to 'REMOVED '.
C
C   Inputs:
C     IOFF       I       Offset of random parameter to delete
C
C   Input/Output in common:
C     CATBLK     I(*)    UV file header
C     CATH       H(*)    UV file header
C     KIPCN      I       CATBLK(KIPCN) is the number of random
C                        parameters
C     KHPTP      I       The list of random parameter names starts at
C                        CATH(KHPTP)
C-----------------------------------------------------------------------
      INTEGER   IOFF

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      IF (IOFF .EQ. CATBLK(KIPCN)-1) THEN
         CATBLK(KIPCN) = CATBLK(KIPCN) - 1
      ELSE
         CALL CHR2H (8, 'REMOVED ', 1, CATH(KHPTP+2*IOFF))
         END IF
C
 999  RETURN
      END
      SUBROUTINE APPRPM (NAME)
C-----------------------------------------------------------------------
C   Append a random parameter with name NAME to the end of the random
C   parameter list.
C
C   Input:
C     NAME       C*8       Name of new random parameter
C
C   Input/output in common:
C     CATBLK     I*(*)     UV data header
C     CATH       H*(*)     UV data header
C     KIPCN      I         CATBLK(KIPCN) is the number of random
C                          parameters
C     KHPTP      I         The list of random parameter names starts at
C                          CATH(KHPTP)
C-----------------------------------------------------------------------
      CHARACTER NAME*8

      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      CALL CHR2H (8, NAME, 1, CATH(KHPTP + 2 * CATBLK(KIPCN)))
      CATBLK(KIPCN) = CATBLK(KIPCN) + 1
C
 999  RETURN
      END
