LOCAL INCLUDE 'RESEQ.INC'
C                                       Local include for RESEQ
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XSOUT, XDISO, XSUB, XANT(50), XBAS(50),
     *   BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, MANT, IANT(50), IBAS(50),
     *   JBUFSZ, ILOCWT, CATOLD(256), INCSI, INCFI, INCIFI, INCSO,
     *   INCFO, INCIFO, LRECI, LRECO, NRPRMI, NRPRMO, THESUB,
     *   IBUFF1(UVBFSS)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      EQUIVALENCE (IBUFF1, BUFF1)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XSUB, XANT, XBAS
      COMMON /PARMS/ CATOLD, SEQIN, SEQOUT, DISKIN, DISKO, ILOCWT,
     *   INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI, LRECO,
     *   NRPRMI, NRPRMO, ISCOMP, IANT, IBAS, MANT, THESUB
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
C                                       End local include for RESEQ
LOCAL END
      PROGRAM RESEQ
C-----------------------------------------------------------------------
C! Allows user to provide subroutine to operate on UV data base
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-2001, 2005, 2007-2008, 2015
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
C   RESEQ renumbers antennas
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
C      SUBARRAY       XSUB          Subarray to be done
C      ANTENNAS       XANT(50)      List of new numbers
C      BASELINE       XBAS(50)      List of old numbers
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'RESEQ.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'RESEQ '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL RESEQI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Read/write uv data, process
      CALL RESEQU (IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Write HI, tables
      CALL RESEQH
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE RESEQI (PRGN, JERR)
C-----------------------------------------------------------------------
C   RESEQI gets input parameters for RESEQ and creates an output file
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
C            /PARMS/ converted input parameters
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX, I, II, JJ
      INCLUDE 'RESEQ.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /'      '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = UVBFSS * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 115
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
      THESUB = XSUB + 0.1
      IF (THESUB.LE.0) THESUB = 1
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
      IF (ISCOMP) THEN
C                                       Find weight and scale.
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
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
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
C                                       antenna list
      MANT = 0
      DO 20 I = 1,50
         II = IROUND (XANT(I))
         JJ = IROUND (XBAS(I))
         IF (JJ.GT.0) THEN
            MANT = MANT + 1
            IANT(MANT) = II
            IBAS(MANT) = JJ
            IF (IANT(MANT).LE.0) IANT(MANT) = IANT(1)
            END IF
 20      CONTINUE
      IF ((MANT.LE.0) .OR. (IANT(1).LE.0)) THEN
         MSGTXT = 'NO ANTENNA NUMBERS WERE SPECIFIED: QUITTING'
         JERR = 5
         GO TO 990
         END IF
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RESEQI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('RESEQI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE RESEQU (IRET)
C-----------------------------------------------------------------------
C   RESEQU sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
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
     *   KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2, INCX, BO, VO,
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY, ISUBA
      LOGICAL   T, F
      INCLUDE 'RESEQ.INC'
      REAL      BASEN, CBUFF(UVBFSS), RESULT(UVBFSS)
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
            IF (ILOCB.GE.0) THEN
               BASEN = BUFF1(IPTRI+ILOCB)
               IA1 = BASEN / 256. + 0.1
               IA2 = BASEN - IA1*256. + 0.1
               BASEN = BASEN - 256 * IA1 - IA2
               ISUBA = 1.5 + 100.0 * BASEN
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               ISUBA = BUFF1(IPTRI+ILOCSA) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
C                                      Call user routine.
C                                       Compressed data.
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL RESEQD (NUMVIS, IA1, IA2, ISUBA, CBUFF,
     *            BUFF1(IPTRI), INCX, RESULT, IRET)
C                                       Un compressed data
            ELSE
               CALL RESEQD (NUMVIS, IA1, IA2, ISUBA,
     *            BUFF1(IPTRI+NRPRMI), BUFF1(IPTRI), INCX, RESULT, IRET)
               END IF
            IF (IRET.GT.0) GO TO 999
C                                       Copy to output.
            XCOUNT = XCOUNT + 1.0D0
            CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
C                                       Compressed
            IF (ISCOMP) THEN
               CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *            BUFF2(IPTRO+NRPRMO))
            ELSE
               CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
               END IF
            IPTRO = IPTRO + LRECO
            NIOUT = NIOUT + 1
            IPTRI = IPTRI + LRECI
C                                       Write vis record.
         IF (NIOUT.GE.NIOLIM) THEN
            CALL UVDISK ('WRIT', LUNO, INDO, BUFF2, NIOLIM, KBIND, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1150) IRET
               GO TO 990
               END IF
            IPTRO = KBIND
            NIOUT = 0
            END IF
 190     CONTINUE
C                                       Read next buffer.
         GO TO 100
C                                       Final call to RESEQD.
 200  NUMVIS = -1
      CALL RESEQD (NUMVIS, IA1, IA2, ISUBA, BUFF1, BUFF1, INCX, RESULT,
     *   IRET)
      IF (IRET.GT.0) THEN
         WRITE (MSGTXT,1120) IRET
         GO TO 990
         END IF
C                                       Finish write
      NIOUT = - NIOUT
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
 1000 FORMAT ('RESEQU: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('RESEQU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('RESEQU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('RESEQU: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('RESEQU: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('RESEQU: RESEQD ERROR',I3)
 1150 FORMAT ('RESEQU: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE RESEQH
C-----------------------------------------------------------------------
C   RESEQH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'RESEQ.INC'
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
         GO TO 100
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, BUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 100
      WRITE (HILINE,1001) TSKNAM, THESUB
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
      IF (IERR.NE.0) GO TO 100
      DO 10 I = 1,MANT
         WRITE (HILINE,1002) TSKNAM, IBAS(I), IANT(I)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 100
 10      CONTINUE
C                                       Close HI file
 100  CALL HICLOS (LUN2, T, BUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                       convert copied tables
      CALL SUBTAB (DISKO, FCNO(NCFILE-1), CATBLK, LUN1, THESUB, MANT,
     *   IBAS, IANT, IBUFF1, IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('RESEQH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1001 FORMAT (A6,'SUBARRAY=',I4,5X,'/ renumber this subarray only')
 1002 FORMAT (A6,'/ antenna',I3,' renumbered as antenna',I3)
 1200 FORMAT ('RESEQH: ERROR COPYING TABLES')
      END
      SUBROUTINE RESEQD (NUMVIS, IA1, IA2, ISUBA, VIS, RPARM, INCX,
     *   RESULT, IRET)
C-----------------------------------------------------------------------
C   Renumbers antennas if appropriate
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      ISUBA   I    Subarray
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Output:
C      RPARM      R    Modified random parameter array.
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, ISUBA, INCX, IRET
      REAL      VIS(INCX,*), RPARM(*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, I, SWPOL,
     *   IROUND
      REAL      RPOL, TMPVIS
      LOGICAL   REVERS
      INCLUDE 'RESEQ.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE SWPOL, NS, NIF, NF
C-----------------------------------------------------------------------
      IRET = 0
C                                       data record
      IF (NUMVIS.GT.0) THEN
C                                       first call
         IF (NUMVIS.EQ.1) THEN
            NS = 1
            NIF = 1
            NF = 1
            IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
            IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
            IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
            DO 10 JS = 1,NS
               RPOL = CATD(KDCRV+JLOCS) + (JS - CATR(KRCRP+JLOCS)) *
     *            CATR(KRCIC+JLOCS)
               I = IROUND (RPOL)
               IF ((I.EQ.-3) .OR. (I.EQ.-7)) THEN
                  IF ((NINT(CATR(KRCIC+JLOCS)).EQ.-1) .AND. (JS.LT.NS))
     *               THEN
                     SWPOL = JS
                  ELSE IF ((NINT(CATR(KRCIC+JLOCS)).EQ.+1) .AND.
     *               (JS.GT.1)) THEN
                     SWPOL = JS - 1
                  ELSE
                     MSGTXT = 'POLARIZATION STRUCTURE DOES NOT PERMIT'
     *                  // ' ANTENNA RENUMBERING'
                     CALL MSGWRT (8)
                     IRET = 8
                     GO TO 999
                     END IF
                  END IF
 10            CONTINUE
            END IF
C                                       desired subarray, check antennas
         REVERS = .FALSE.
         IF (THESUB.EQ.ISUBA) THEN
            DO 15 I = 1,MANT
               IF (IBAS(I).EQ.IA1) THEN
                  IA1 = IANT(I)
                  GO TO 20
                  END IF
 15            CONTINUE
 20         DO 25 I = 1,MANT
               IF (IBAS(I).EQ.IA2) THEN
                  IA2 = IANT(I)
                  GO TO 30
                  END IF
 25            CONTINUE
 30         IF (IA2.LT.IA1) THEN
               REVERS = .TRUE.
               I = IA2
               IA2 = IA1
               IA1 = I
               RPARM(1+ILOCU) = -RPARM(1+ILOCU)
               RPARM(1+ILOCV) = -RPARM(1+ILOCV)
               RPARM(1+ILOCW) = -RPARM(1+ILOCW)
               END IF
            IF (ILOCB.GE.0) THEN
               RPARM(1+ILOCB) = 256 * IA1 + IA2 + 0.01 * (ISUBA-1)
            ELSE
               RPARM(1+ILOCA1) = IA1
               RPARM(1+ILOCA2) = IA2
               RPARM(1+ILOCSA) = ISUBA
               END IF
            END IF
C                                       copy data
         DO 70 JIF = 1,NIF
            DO 60 JF = 1,NF
               DO 50 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
C                                       swap crosshand at swpol
                  IF ((REVERS) .AND. (JS.EQ.SWPOL)) THEN
                     DO 35 I = 1,INCX
                        TMPVIS = VIS(I,INDEXI)
                        VIS(I,INDEXI) = VIS(I,INDEXI+INCSI)
                        VIS(I,INDEXI+INCSI) = TMPVIS
 35                     CONTINUE
                     END IF
C                                       copy to output
                  DO 40 I = 1,INCX
                     RESULT(I,INDEXO) = VIS(I,INDEXI)
 40                  CONTINUE
                  IF (REVERS) RESULT(2,INDEXO) = -RESULT(2,INDEXO)
 50               CONTINUE
 60            CONTINUE
 70         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE SUBTAB (IDISK, ICNO, CATBLK, JLUN, SUBA, MANT, INANT,
     *   OUTANT, JBUFF, IRET)
C----------------------------------------------------------------------
C   Update antenna numbers in generic tables attached to uv-data file
C   Input:
C      IDISK    I      Disk volume number
C      ICNO     I      Catalog slot number
C      JLUN     I      LUN for table I/O
C      SUBA     I      subarray to change
C      MANT     I      Number of antennas to change
C      INANT    I(*)   Input antenna numbers
C      OUTANT   I(*)   Output antenna numbers
C   Output:
C      JBUFF    I(512) Buffer for table I/O
C      IRET     I      Return code (0=> ok; else error)
C----------------------------------------------------------------------
      INTEGER   IDISK, ICNO, CATBLK(256), JLUN, SUBA, MANT, INANT(*),
     *   OUTANT(*), JBUFF(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NCOLMX, NSUBID, NANTID
      PARAMETER (NSUBID = 2, NANTID = 7)
      PARAMETER (NCOLMX = 7)
C
      LOGICAL   WTABLE, WEXIST, WFITS, T
      CHARACTER LTYPE*2, LSUBID(NSUBID)*24, LANTID(NANTID)*24
      DOUBLE PRECISION DREC(XBPRSZ/2)
      REAL      RECR(XBPRSZ)
      INTEGER   ITAB, NKEY, NREC, NCOL, IVER, NVER, DATP(128,2), I,
     *   IERR, IKOLS(NCOLMX), J, IANT, JANT, NROW, IROW, IRNO,
     *   IREC(XBPRSZ), ISUBA, JSUBA
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (DREC, RECR, IREC)
      DATA LSUBID /'ARRAY', 'SUBARRAY'/
      DATA LANTID /'ANTENNA_NO', 'ANTENNA_NOS', 'ANTENNA',
     *   'ANTENNA NO.', 'ANTS', 'ANTENNA NO','NOSTA'/
      DATA T /.TRUE./
C----------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Loop over all table types
      DO 100 ITAB = 1,KIEXTN
C                                       Extract type, version.
         CALL H2CHR (2, 1, CATBLK(KHEXT+ITAB-1), LTYPE)
         NVER = CATBLK(KIVER+ITAB-1)
C                                       Loop over all table versions
         DO 90 IVER = 1,NVER
C                                       valid, existing table ?
            CALL ISTAB (LTYPE, IDISK, ICNO, IVER, JLUN, JBUFF,
     *         WTABLE, WEXIST, WFITS, IERR)
C                                       Skip if invalid
            IF ((IERR.NE.0) .OR. (.NOT.WEXIST) .OR. (.NOT.WTABLE))
     *         GO TO 90
C                                       If its an AN table, but its the
C                                       wrong one, skip table
            IF ((LTYPE.EQ.'AN') .AND. (IVER.NE.SUBA)) GO TO 90
C
C                                       Open table
            CALL TABINI ('READ', LTYPE, IDISK, ICNO, IVER, CATBLK,
     *         JLUN, NKEY, NREC, NCOL, DATP, JBUFF, IERR)
            IF (IERR.NE.0) THEN
               IRET = 1
               WRITE (MSGTXT,1000) IERR, LTYPE, IVER
               GO TO 990
               END IF
C                                       Search for antennas column
            CALL FNDCOL (NANTID, LANTID, 24, T, JBUFF, IKOLS, IERR)
            IF ((IERR.GE.1) .AND. (IERR.LE.10)) THEN
               IRET = 2
               WRITE (MSGTXT,1010) IERR, LTYPE, IVER
               GO TO 990
               END IF
C                                       Match found ?
            JANT = 0
            DO 10 J = 1, NANTID
               IF ((IKOLS(J).GT.0) .AND. (JANT.EQ.0)) JANT = IKOLS(J)
 10            CONTINUE
            IF (JANT.EQ.0) GO TO 80
C                                       If this is an AN table,
C                                       then dont look for subarray column
            IF (LTYPE.EQ.'AN') THEN
               JSUBA = 0
C                                       find subarray column
            ELSE
               CALL FNDCOL (NSUBID, LSUBID, 24, T, JBUFF, IKOLS, IERR)
               IF ((IERR.GE.1) .AND. (IERR.LE.10)) THEN
                  IRET = 2
                  WRITE (MSGTXT,1010) IERR, LTYPE, IVER
                  GO TO 990
                  END IF
C                                       Match found ?
               JSUBA = 0
               DO 20 J = 1,NSUBID
                  IF ((IKOLS(J).GT.0).AND.(JSUBA.EQ.0)) JSUBA = IKOLS(J)
 20               CONTINUE
               END IF
C                                       Message about table update
            WRITE (MSGTXT,1020) LTYPE, IVER
            CALL MSGWRT (4)
C                                       Close table; open for
C                                       re-write
            CALL TABIO ('CLOS', 0, 0, JBUFF, JBUFF, IERR)
            CALL TABINI ('WRIT', LTYPE, IDISK, ICNO, IVER, CATBLK,
     *         JLUN, NKEY, NREC, NCOL, DATP, JBUFF, IERR)
            IF (IERR.NE.0) THEN
               IRET = 1
               WRITE (MSGTXT,1000) IERR, LTYPE, IVER
               GO TO 990
               END IF
C                                       Loop through table
            NROW = JBUFF(5)
            DO 50 IROW = 1,NROW
C                                       Read record
               IRNO = IROW
               CALL TABIO ('READ', 0, IRNO, RECR, JBUFF, IERR)
               IF (IERR.NE.0) THEN
                  IRET = 3
                  WRITE (MSGTXT,1010) IERR, LTYPE, IVER
                  GO TO 990
                  END IF
C                                       Extract antenna number
               IANT = IREC(DATP(JANT,1))
C                                       if this table has a subarray
C                                       column
               IF (JSUBA.GT.0) THEN
                  ISUBA = IREC(DATP(JSUBA,1))
                  IF (ISUBA.NE.SUBA) GO TO 50
                  END IF
C                                       is it special?
               DO 30 I = 1,MANT
                  IF (IANT.EQ.INANT(I)) THEN
                     IANT = OUTANT(I)
                     GO TO 40
                     END IF
 30               CONTINUE
C                                       change to new number
 40            IREC(DATP(JANT,1)) = IANT
C                                       Re-write record
               IRNO = IROW
               CALL TABIO ('WRIT', 0, IRNO, RECR, JBUFF, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1030) IERR, LTYPE, IVER
                  GO TO 990
                  END IF
 50            CONTINUE
C                                       Mark table as unsorted
            JBUFF(43) = 0
            JBUFF(44) = 0

C                                       Close table
 80         CALL TABIO ('CLOS', 0, 0, JBUFF, JBUFF, IERR)
 90         CONTINUE
 100     CONTINUE
C
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1000  FORMAT ('SUBTAB: ERR',I3,' OPENING ',A2,' TABLE',I4)
1010  FORMAT ('SUBTAB: ERR',I3,' READING ',A2,' TABLE',I4)
1020  FORMAT ('Updating ',A2,' table; version', I4)
1030  FORMAT ('SUBTAB: ERR',I3,' WRITING ',A2,' TABLE',I4)
      END
