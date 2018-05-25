LOCAL INCLUDE 'DSORC.INC'
C                                       Local include for DSORC
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XSOUT, XDISO, APARM(10), BPARM(10),
     *   XCENT, BUFF1(UVBFSS), BUFF2(UVBFSS), DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, IAPARM(10), IBPARM(10), NCHANG(10),
     *   IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      DOUBLE PRECISION UVSCAL
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, APARM, BPARM, XCENT
      COMMON /OTHRPM/ CATOLD, UVSCAL, SEQIN, SEQOUT, DISKIN, DISKO,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, ISCOMP, IAPARM, IBPARM, NCHANG, DIFPIX
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
C                                       End local include for DSORC
LOCAL END
      PROGRAM DSORC
C-----------------------------------------------------------------------
C! Allows user to provide subroutine to operate on UV data base
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 2004-2005, 2008, 2011, 2014-2015
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
C   DSORC allows a user to renumber sources in a multi-source file,
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C      OUTNAME        NAMOUT        Name of the output uv file.
C                                   Default output is input file.
C      OUTCLASS       CLAOUT        Class of the output uv file.
C      OUTSEQ         SEQOUT        Seq. number of output uv data.
C      OUTDISK        DISKO         Disk number of the output file.
C      APARM(10)      IAPARM        User specified array.
C      BPARM(10)      IBPARM        User specified array.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'DSORC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'DSORC '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL DSORIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL DSORUV (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       HI and table source #s
      CALL DSORHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE DSORIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   DSORIN gets input parameters for DSORC and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Output in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C   See prologue comments in DSORC for more details.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX, I
      LOGICAL   T
      INCLUDE 'DSORC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /'      '/
      DATA T /.TRUE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (T)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 35
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
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
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       check numbers
      DO 10 I =1,10
         IAPARM(I) = IROUND (APARM(I))
         IBPARM(I) = IROUND (BPARM(I))
         IF (IBPARM(I).LE.0) IBPARM(I) = 1
         NCHANG(I) = 0
 10      CONTINUE
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1040) IERR
         GO TO 990
         END IF
C                                       Save input CATBLK
      CALL COPY (256, CATBLK, CATOLD)
C                                       Compressed data?
      ISCOMP = CATBLK(KINAX).EQ.1
C                                       Find weight and scale.
      IF (ISCOMP) THEN
         CALL AXEFND (8, 'WEIGHT  ', CATBLK(KIPCN), CATH(KHPTP), ILOCWT,
     *      JERR)
         IF (JERR.NE.0) THEN
            MSGTXT = 'ERROR FINDING WEIGHT FOR COMPRESSED DATA'
            JERR = 9
            GO TO 990
            END IF
         END IF
C                                       Get uv header info.
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       is it multi-source?
      IF (ILOCSU.LT.0) THEN
         MSGTXT = 'IS ONLY FOR MULTI-SOURCE FILES'
         JERR = 9
         GO TO 990
         END IF
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
      IF (JLOCF.LT.0) XCENT = -1.0
      IF (XCENT.LE.0.0) THEN
         UVSCAL = 1.0D0
         DIFPIX = 0.09
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
C                                       Create output file.
      CCNO = 1
      FRW(NCFILE+1) = 3
      JERR = 4
      CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1050) IERR
            GO TO 990
            END IF
C                                       Only overwrite Input file
C                                       no destroy existing otherwise
         IF ((CCNO.NE.OLDCNO) .OR. (DISKO.NE.DISKIN)) THEN
            WRITE (MSGTXT,1060)
            GO TO 990
            END IF
C                                       Recover existing CATBLK
         FRW(NCFILE+1) = 2
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', BUFF1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1065) IERR
            CALL MSGWRT (6)
            END IF
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = FRW(NCFILE) - 1
C                                       Save output file info
      CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
      INCX = CATBLK(KINAX)
      LRECO = LREC
      NRPRMO = NRPARM
      INCSO = INCS / INCX
      INCFO = INCF / INCX
      INCIFO = INCIF / INCX
C                                        Put input file in READ
      PTYPE = 'UV'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
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
 1000 FORMAT ('DSORIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('DSORIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE DSORUV (IRET)
C-----------------------------------------------------------------------
C   DSORUV reads and writes the uv data one point at a time, correcting
C   the source numbers.
C   Input in common:
C      LRECI   I  Input file record length
C      NRPRMI  I  Input number of random parameters.
C      INCSI   I  Input Stokes' increment in vis.
C      INCFI   I  Input frequency increment in vis.
C      INCIFI  I  Input IF increment in vis.
C      LRECO   I  Output file record length
C      NRPRMO  I  Output number of random parameters.
C      INCSO   I  Output Stokes' increment in vis.
C      INCFO   I  Output frequency increment in vis.
C      INCIFO  I  Output IF increment in vis.
C      ISCOMP  L  If true data is compressed
C   Output:
C      IRET    I  Return code, 0 => OK, otherwise abort.
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      CHARACTER OFILE*48, IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, ILENBU,
     *   KBIND, NIOUT, NIOLIM, IBIND, I, IS, INCX, BO, VO, XCOUNT, J,
     *   NCORI, NCORO, NCOPY
      LOGICAL   T, F
      INCLUDE 'DSORC.INC'
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
C                                       Dimension of complex axis
      INCX = CATBLK(KINAX)
      IF (ISCOMP) INCX = 3
C                                       Number of visibilities in input
C                                       and output files.
      NCORI = (LRECI - NRPRMI) / CATOLD(KINAX)
      NCORO = (LRECO - NRPRMO) / CATBLK(KINAX)
      NCOPY = LRECO - NRPRMO
C                                       Open and init for read
C                                       visibility file
      CALL ZPHFIL ('UV', DISKIN, FCNO(NCFILE), 1, IFILE, IRET)
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
      CALL UVINIT ('WRIT', LUNO, INDO, NVIS, VO, LRECO, ILENBU, JBUFSZ,
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
      CALL UVINIT ('READ', LUNI, INDI, NVIS, VO, LRECI, ILENBU, JBUFSZ,
     *   BUFF1, BO, IBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1030) IRET
         GO TO 990
         END IF
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
         DO 190 J = 1,INIO
            BUFF1(IPTRI+ILOCU) = BUFF1(IPTRI+ILOCU) * UVSCAL
            BUFF1(IPTRI+ILOCV) = BUFF1(IPTRI+ILOCV) * UVSCAL
            BUFF1(IPTRI+ILOCW) = BUFF1(IPTRI+ILOCW) * UVSCAL
C                                       copy to output
            CALL RCOPY (LRECI, BUFF1(IPTRI), BUFF2(IPTRO))
C                                       fix source
            IS = BUFF2(IPTRO+ILOCSU) + 0.01
            DO 120 I = 1,10
               IF (IS.EQ.IAPARM(I)) THEN
                  BUFF2(IPTRO+ILOCSU) = IBPARM(I)
                  NCHANG(I) = NCHANG(I) + 1
                  GO TO 125
                  END IF
 120           CONTINUE
C                                       count, move pointers
 125        XCOUNT = XCOUNT + 1
            NIOUT = NIOUT + 1
            IPTRO = IPTRO + LRECO
            IPTRI = IPTRI + LRECI
C                                       Write vis record.
            IF (NIOUT.GE.NIOLIM) THEN
               CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND,
     *            IRET)
               IF (IRET.NE.0) THEN
                  WRITE (MSGTXT,1150) IRET
                  GO TO 990
                  END IF
               IPTRO = KBIND
               NIOUT = 0
               END IF
 190        CONTINUE
C                                       Read next buffer.
         GO TO 100
C                                       Finish write
 200  NIOUT = - NIOUT
      CALL UVDISK ('FLSH', LUNO, INDO, BUFF2, NIOUT, KBIND, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1150) IRET
         GO TO 990
         END IF
C                                       Compress output file.
      NVIS = XCOUNT
      CALL UCMPRS (NVIS, DISKO, CCNO, LUNO, CATBLK, IRET)
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
 1000 FORMAT ('DSORUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('DSORUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('DSORUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('DSORUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('DSORUV: ERROR',I3,' READING VIS FILE')
 1150 FORMAT ('DSORUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE DSORHI
C-----------------------------------------------------------------------
C   DSORHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, NONOT, INV, OUTV
      LOGICAL   T
      INCLUDE 'DSORC.INC'
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
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, BUFF1, BUFF2, IERR)
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
C                                      Add new history.
      DO 50 I = 1,10
         IF (NCHANG(I).GT.0) THEN
            WRITE (HILINE,1010) TSKNAM, IAPARM(I), IBPARM(I), NCHANG(I)
            CALL HIADD (LUN2, HILINE, BUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
            MSGTXT = HILINE(7:)
            CALL MSGWRT (4)
            END IF
 50      CONTINUE
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                       copy NX table
      INV = 1
      OUTV = 1
      CALL TABCOP ('NX', INV, OUTV, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
C                                       rearrange source numbers
      CALL DSOUTA (LUN1, LUN2, DISKO, FCNO(1), IAPARM, IBPARM, IBUFF1,
     *   IBUFF2, IERR)
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, FCNO(1), DIFPIX, BUFF1, BUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'CENTHI: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('DSORHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'INSORC=',I4,'  OUTSORC=',I4,'  / renumber',I8,
     *   ' vis records')
 1200 FORMAT ('DSORHI: ERROR COPYING TABLES')
      END
      SUBROUTINE DSOUTA (LUN1, LUN2, VOL, CNO, IAPARM, IBPARM, BUFF1,
     *   BUFF2, IRET)
C-----------------------------------------------------------------------
C   DSOUTA re-writes tables correcting the source numbers if any.
C   Inputs:
C      LUN1     I        LUN for read
C      LUN2     I        LUN for write
C      VOL      I        Disk number
C      CNO      I        Catalog slot number
C      IAPARM   I(10)    List of input source numbers
c      IAPARM   I(10)    List of input source numbers
C   Output:
C      BUFF1    I(512)   Work buffer
C      BUFF2    I(512)   Work buffer
C      IRET     I        Return error code  0 => ok
C                           1 => files the same, no copy.
C                           2 => no input files exist
C                           3 => failed
C                           4 => no output files created.
C                           5 => failed to update CATBLK
C-----------------------------------------------------------------------
      INTEGER   LUN1, LUN2, VOL, CNO, IAPARM(10), IBPARM(10), BUFF1(*),
     *   BUFF2(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER SIDKEY(3)*24, TYPE*2
      REAL      RECR(3*MAXCIF)
      DOUBLE PRECISION RECD(512)
      INTEGER   NVERS, NKEY, NREC, NCOL, DATP(128,2,2), IER, I, SOUKOL,
     *   RECORD(3*MAXCIF), SUID, NRECIN, INREC, OUTREC, LOOP, INVER,
     *   OUTVER, ITYPE, ISPARM(10)
      LOGICAL   T, F, TABLE, EXIST, FITASC, OPEN1, OPEN2
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
C                                       3rd one for SU tables
C                                       SU also has 2nd w different use
      DATA SIDKEY /'SOURCE_ID ', 'SOURCE ', 'ID. NO. '/
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       loop over types
      DO 100 ITYPE = 1,KIEXTN
         CALL H2CHR (2, 1, CATH(KHEXT+ITYPE-1), TYPE)
         NVERS = CATBLK(KIVER+ITYPE-1)
         DO 90 INVER = 1,NVERS
            OPEN1 = F
            OPEN2 = F
            EXIST = .FALSE.
            IRET = 0
            CALL COPY (10, IAPARM, ISPARM)
            IF ((TYPE.NE.'HI') .AND. (TYPE.NE.'PL') .AND. (TYPE.NE.'SL')
     *         .AND. (TYPE.NE.' ')) CALL ISTAB (TYPE, VOL, CNO, INVER,
     *         LUN1, BUFF1, TABLE, EXIST, FITASC, IRET)
C                                       special SU table stuff
            IF ((IRET.EQ.0) .AND. (EXIST) .AND. (TABLE) .AND.
     *         (TYPE.EQ.'SU')) THEN
C                                       Open first (input) file
               CALL TABINI ('READ', TYPE, VOL, CNO, INVER, CATBLK, LUN1,
     *            NKEY, NREC, NCOL, DATP(1,1,1), BUFF1, IER)
C                                       If not there - quit
               IF (IER.GT.0) GO TO 90
               OPEN1 = T
               CALL FNDCOL (1, SIDKEY(3), 8, T, BUFF1, SOUKOL, IER)
               IF (IER.NE.0) GO TO 90
               NRECIN = BUFF1(5)
               SOUKOL = DATP(SOUKOL,1,1)
C                                       If IBPARM already in table
C                                       do not update that one
               DO 20 LOOP = 1,NRECIN
                  INREC = LOOP
                  CALL TABIO ('READ', 0, INREC, RECORD, BUFF1, IER)
                  IF (IER.GT.0) THEN
                     WRITE (MSGTXT,1070) IER, 'READ', TYPE
                     GO TO 990
                     END IF
C                                       Source translate
                  SUID = RECORD(SOUKOL)
                  IF (SUID.GT.0) THEN
                     DO 10 I = 1,10
                        IF (SUID.EQ.IBPARM(I)) THEN
                           ISPARM(I) = -99999
                           WRITE (MSGTXT,1000) IAPARM(I), IBPARM(I)
                           CALL MSGWRT (6)
                           END IF
 10                     CONTINUE
                     END IF
 20               CONTINUE
               IF (OPEN1) THEN
                  CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
                  IF (IER.GT.0) THEN
                     WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
                     GO TO 990
                     END IF
                  END IF
               OPEN1 = F
               END IF
            IF ((IRET.EQ.0) .AND. (EXIST) .AND. (TABLE)) THEN
C                                       Open first (input) file
               CALL TABINI ('READ', TYPE, VOL, CNO, INVER, CATBLK, LUN1,
     *            NKEY, NREC, NCOL, DATP(1,1,1), BUFF1, IER)
C                                       If not there - quit
               IF (IER.GT.0) GO TO 90
               OPEN1 = T
C                                       Find source id column.
               IF (TYPE.EQ.'SU') THEN
                  CALL FNDCOL (1, SIDKEY(3), 8, T, BUFF1, SOUKOL, IER)
               ELSE
                  CALL FNDCOL (1, SIDKEY(1), 10, T, BUFF1, SOUKOL, IER)
                  IF (IER.NE.0) CALL FNDCOL (1, SIDKEY(2), 7, T, BUFF1,
     *               SOUKOL, IER)
                  END IF
C                                       quit if no source column
               IF (IER.EQ.0) THEN
C                                       Open second (output) file
                  OUTVER = INVER
                  CALL TABINI ('WRIT', TYPE, VOL, CNO, OUTVER, CATBLK,
     *               LUN2, NKEY, NREC, NCOL, DATP(1,1,2), BUFF2, IER)
                  IF (IER.GT.0) GO TO 80
                  OPEN2 = T
C                                       Get number of records in the
C                                       files.
                  NRECIN = BUFF1(5)
                  OUTREC = 1
C                                       Mark unsorted
                  IF ((ABS(BUFF2(43)).EQ.SOUKOL) .OR.
     *               (ABS(BUFF2(44)).EQ.SOUKOL)) THEN
                     BUFF2(43) = 0
                     BUFF2(44) = 0
                     END IF
C                                       translate to physical plaice in
C                                       record
                  SOUKOL = DATP(SOUKOL,1,1)
C                                       Copy
                  DO 70 LOOP = 1,NRECIN
                     INREC = LOOP
                     CALL TABIO ('READ', 0, INREC, RECORD, BUFF1, IER)
                     IF (IER.GT.0) THEN
                        WRITE (MSGTXT,1070) IER, 'READ', TYPE
                        GO TO 990
                        END IF
C                                       Source translate
                     SUID = RECORD(SOUKOL)
                     IF (SUID.GT.0) THEN
                        DO 50 I = 1,10
                           IF (SUID.EQ.ISPARM(I)) THEN
                              RECORD(SOUKOL) = IBPARM(I)
                              GO TO 60
                              END IF
 50                        CONTINUE
                        END IF
 60                  CALL TABIO ('WRIT', 0, OUTREC, RECORD, BUFF2, IER)
                     IF (IER.GT.0) THEN
                        WRITE (MSGTXT,1070) IER, 'WRIT', TYPE
                        GO TO 990
                        END IF
                     OUTREC = OUTREC + 1
 70                  CONTINUE
                  END IF
C                                       Close tables.
 80            IF (OPEN1) THEN
                  CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
                  IF (IER.GT.0) THEN
                     WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
                     GO TO 990
                     END IF
                  END IF
               OPEN1 = F
               IF (OPEN2) THEN
                  CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
                  IF (IER.GT.0) THEN
                     WRITE (MSGTXT,1070) IER, 'CLOS', TYPE
                     GO TO 990
                     END IF
                  END IF
               END IF
 90         CONTINUE
 100     CONTINUE
      IRET = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
      IF (OPEN1) CALL TABIO ('CLOS', 0, INREC, RECORD, BUFF1, IER)
      IF (OPEN2) CALL TABIO ('CLOS', 0, OUTREC, RECORD, BUFF2, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('Source',I4,' not renumbered to',I4,' in SU table',
     *   ' it is already there')
 1070 FORMAT ('DSOUTA: ERROR ',I3,2X,A4,'ING ',A2,' TABLE')
      END
