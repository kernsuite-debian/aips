LOCAL INCLUDE 'FIXRL.INC'
C                                       Local include for FIXRL
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XSOUT, XDISO, VPARM(30), DOTABL, XCENT,
     *   BUFF1(UVBFSS), BUFF2(UVBFSS), DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, ILOCWT, OLDCNO,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, NUMANT, ANTS(30), NEWCNO,
     *   IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      LOGICAL   ISCOMP
      DOUBLE PRECISION UVSCAL
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, VPARM, DOTABL, XCENT
      COMMON /OTHPRM/ CATOLD, UVSCAL, SEQIN, SEQOUT, DISKIN, DISKO,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, ISCOMP, NUMANT, ANTS, OLDCNO, NEWCNO,
     *   DIFPIX
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
C                                       End local include for FIXRL
LOCAL END
      PROGRAM FIXRL
C-----------------------------------------------------------------------
C! Swaps R and L in some antennas
C# Utility UV
C-----------------------------------------------------------------------
C;  Copyright (C) 2012-2015
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
C   FIXRL swaps R and L for miswired antennas
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
C      VPARM(30)      VPARM         List of antennas
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'FIXRL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'FIXRL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FIXRLI (PRGM, IRET)
C                                       Call routine that sends data
C                                       to the user routine.
      IF (IRET.EQ.0) CALL FIXRLU (IRET)
      IF (IRET.EQ.0) CALL FIXRLH
C                                       Close down files, etc.
      CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FIXRLI (PRGN, JERR)
C-----------------------------------------------------------------------
C   FIXRLI gets input parameters for FIXRL and creates an output file
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
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   IROUND, NPARM, IERR, INCX, I
      INCLUDE 'FIXRL.INC'
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
      NPARM = 46
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
      IF (JLOCF.LT.0) XCENT = -1.
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
      NEWCNO = CCNO
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
      NUMANT = 0
      DO 10 I = 1,30
         IF (VPARM(I).GT.0.5) THEN
            NUMANT = NUMANT + 1
            ANTS(NUMANT) = VPARM(I) + 0.5
         ELSE
            GO TO 999
            END IF
 10      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIXRLI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('FIXRLI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE FIXRLU (IRET)
C-----------------------------------------------------------------------
C   FIXRLU sends uv data one point at a time to the user supplied
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
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY, RNXRET
      LOGICAL   T, F
      INCLUDE 'FIXRL.INC'
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
C                                       make an index table
      CALL RNXGET (DISKIN, OLDCNO, CATOLD)
      CALL RNXINI (DISKO, NEWCNO, CATBLK, RNXRET)
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
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            BUFF1(IPTRI+ILOCU) = BUFF1(IPTRI+ILOCU) * UVSCAL
            BUFF1(IPTRI+ILOCV) = BUFF1(IPTRI+ILOCV) * UVSCAL
            BUFF1(IPTRI+ILOCW) = BUFF1(IPTRI+ILOCW) * UVSCAL
            NUMVIS = NUMVIS + 1
C                                      Call user routine.
            IF (ISCOMP) THEN
C                                       Compressed data.
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL DIDDLE (NUMVIS, IA1, IA2, CBUFF, INCX, RESULT, IRET)
            ELSE
C                                       Un compressed data
               CALL DIDDLE (NUMVIS, IA1, IA2, BUFF1(IPTRI+NRPRMI),
     *            INCX, RESULT, IRET)
            END IF
C                                       Branch on his return
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1.0D0
C                                       update NX table
               CALL RNXUPD (BUFF1(IPTRI), RNXRET)
C                                       random parms
               CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
C                                       data: compressed
               IF (ISCOMP) THEN
                  CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
C                                       data: uncompressed
               ELSE
                  CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
               END IF
C                                       OK, but no output please
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
      CALL RNXCLS (RNXRET)
      IF (RNXRET.NE.0) THEN
         MSGTXT = 'OUTPUT NX TABLE, IF ANY, IS INCOMPLETE'
         GO TO 990
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIXRLU: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('FIXRLU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('FIXRLU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('FIXRLU: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('FIXRLU: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('FIXRLU: DIDDLE ERROR',I3)
 1150 FORMAT ('FIXRLU: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE FIXRLH
C-----------------------------------------------------------------------
C   FIXRLH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP(3)*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      INCLUDE 'FIXRL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA NONOT, NOTTYP /3, 'SY', 'TY', 'CD'/
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
      DO 20 I = 1,NUMANT
         WRITE (HILINE,1010) TSKNAM, I, ANTS(I)
         CALL HIADD (LUN2, HILINE, BUFF2, IERR)
         IF (IERR.NE.0) GO TO 200
 20      CONTINUE
C                                       Close HI file
 200  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                        Copy tables
      IF (DOTABL.LT.0.0) NONOT = 0
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                       fix tables
      IF (DOTABL.GE.0.0) THEN
         CALL FIXTAB (NUMANT, ANTS, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *      FCNO(1), CATOLD, CATBLK, IBUFF1, IBUFF2, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1210) IERR
            CALL MSGWRT (7)
            END IF
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, FCNO(1), DIFPIX, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'FIXRLH: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIXRLH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'VPARM(',I2,') = ',I3,'   / antenna pol swapped')
 1200 FORMAT ('FIXRLH: ERROR COPYING TABLES')
 1210 FORMAT ('FIXRLH: ERROR',I4,' FIXING SY, TY, CD TABLES')
      END
      SUBROUTINE DIDDLE (NUMVIS, IA1, IA2, VIS, INCX, RESULT, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      NUMANT     I       size of antenna list
C      ANTS       I(30)   antenna list
C      RA         D       Right ascension (1950) of phase center. (deg)
C      DEC        D       Declination (1950) of phase center. (deg)
C      FREQ       D       Frequency of observation (Hz)
C      NRPARM     I       # random parameters.
C      NCOR       I       # correlators
C      CATBLK     I(256)  Catalog header record. See Going Aips for
C                         details.
C      LRECI      I    Input file record length
C      NRPRMI     I    Input number of random parameters.
C      INCSI      I    Input Stokes' increment in vis.
C      INCFI      I    Input frequency increment in vis.
C      INCIFI     I    Input IF increment in vis.
C      LRECO      I    Output file record length
C      NRPRMO     I    Output number of random parameters.
C      INCSO      I    Output Stokes' increment in vis.
C      INCFO      I    Output frequency increment in vis.
C      INCIFO     I    Output IF increment in vis.
C   Output:
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C
C   Output in COMMON:
C      NUMHIS    I         # history entries (max. 10)
C      HISCRD    C(NUMHIS) History records
C      CATBLK    I         Catalog header block
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, IA1, IA2, INCX, IRET
      REAL      VIS(INCX,*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, I, OS(4,4), KS
      LOGICAL   DO1, DO2
      INCLUDE 'FIXRL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA OS /2,1,4,3, 4,3,2,1, 3,4,1,2, 1,2,3,4/
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
C                                       which swap
         DO1 = .FALSE.
         DO2 = .FALSE.
         DO 10 I = 1,NUMANT
            IF (IA1.EQ.ANTS(I)) DO1 = .TRUE.
            IF (IA2.EQ.ANTS(I)) DO2 = .TRUE.
 10         CONTINUE
         IF ((DO1) .AND. (DO2)) THEN
            KS = 1
         ELSE IF (DO1) THEN
            KS = 2
         ELSE IF (DO2) THEN
            KS = 3
         ELSE
            KS = 4
            END IF
C                                       now do it
         DO 60 JIF = 1,NIF
            DO 50 JF = 1,NF
               DO 40 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
C                                       swap pol w OS array
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (OS(JS,KS)-1) * INCSO + 1
                  DO 30 I = 1,INCX
                     RESULT(I,INDEXO) = VIS(I,INDEXI)
 30                  CONTINUE
 40               CONTINUE
 50            CONTINUE
 60         CONTINUE
         END IF
C
 999  RETURN
      END
      SUBROUTINE FIXTAB (NUMANT, ANTS, LUNI, LUNO, DISKI, DISKO, CNOI,
     *   CNOO, CATOLD, CATBLK, BUFF1, BUFF2, IERR)
C-----------------------------------------------------------------------
C   swap SY, TY, CD tables, copying to output
C   Inputs:
C      NUMANT   I      Number in ANTS
C      ANTS     I(*)   Antennas to swap
C      LUNI     I      Input LUN
C      LUNO     I      Output LUN
C      DISKI    I      Input disk
C      DISKO    I      Output disk
C      CNOI     I      Input catalog number
C      CNOO     I      Input catalog number
C      CATOLD   I(*)   Input header
C   In/Out
C      CATBLK   I(*)   Output header
C      BUFF1    I(*)   buffer 1
C      BUFF2    I(*)   buffer 2
C   Output:
C      IERR     I      error code
C-----------------------------------------------------------------------
      INTEGER   NUMANT, ANTS(*),  LUNI, LUNO, DISKI, DISKO, CNOI,
     *   CNOO, CATOLD(256), CATBLK(256), BUFF1(*), BUFF2(*), IERR
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NVER, NREC, IVER, RNO, KOLS(20), NUMV(20), NUMA, I,
     *   NUMPOL, NUMIF, IREC, ANTNO, SUBA, FREQID, SOURID
      CHARACTER RDATE*8
      REAL      TIMEI, TCAL(2,MAXIF), PDIFF(2,MAXIF), PSUM(2,MAXIF),
     *   PGAIN(2,MAXIF), TSYS(2,MAXIF), TANT(2,MAXIF), TEMP
      DOUBLE PRECISION TIME
      LOGICAL   SWAP
      INCLUDE 'INCS:DMSG.INC'
C-----------------------------------------------------------------------
C                                       CD tables
      CALL FNDEXT ('CD', CATOLD, NVER)
      IF (NVER.GT.0) THEN
         DO 100 IVER = 1,NVER
            CALL CDINI ('READ', BUFF1, DISKI, CNOI, IVER, CATOLD, LUNI,
     *         RNO, KOLS, NUMV, NUMA, NUMPOL, NUMIF, RDATE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN INPUT CD', IVER
               CALL MSGWRT (7)
               GO TO 100
               END IF
            NREC = BUFF1(5)
            CALL CDINI ('WRIT', BUFF2, DISKO, CNOO, IVER, CATBLK, LUNO,
     *         RNO, KOLS, NUMV, NUMA, NUMPOL, NUMIF, RDATE, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT CD', IVER
               CALL MSGWRT (7)
               GO TO 95
               END IF
            DO 50 IREC = 1,NREC
               RNO = IREC
               CALL TABCD ('READ', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, ANTNO, SUBA, FREQID, TCAL, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ INPUT CD', IVER
                  CALL MSGWRT (7)
                  GO TO 90
                  END IF
               SWAP = .FALSE.
               DO 20 I = 1,NUMANT
                  IF (ANTS(I).EQ.ANTNO) SWAP = .TRUE.
 20               CONTINUE
               IF (SWAP) THEN
                  DO 30 I = 1,NUMIF
                     TEMP = TCAL(1,I)
                     TCAL(1,I) = TCAL(2,I)
                     TCAL(2,I) = TEMP
 30                  CONTINUE
                  END IF
               RNO = IREC
               CALL TABCD ('WRIT', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, ANTNO, SUBA, FREQID, TCAL, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ OUTPUT CD', IVER
                  CALL MSGWRT (7)
                  GO TO 90
                  END IF
 50            CONTINUE
            WRITE (MSGTXT,1050) 'CD', DISKI, CNOI, IVER, DISKO, CNOO,
     *         IVER
            CALL MSGWRT (3)
C                                       close
 90         RNO = NREC
            CALL TABCD ('CLOS', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, ANTNO, SUBA, FREQID, TCAL, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE OUTPUT CD', IVER
               CALL MSGWRT (7)
               END IF
 95         RNO = NREC
            CALL TABCD ('CLOS', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, ANTNO, SUBA, FREQID, TCAL, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE INPUT CD', IVER
               CALL MSGWRT (7)
               END IF
 100        CONTINUE
         END IF
C                                       SY tables
      CALL FNDEXT ('SY', CATOLD, NVER)
      IF (NVER.GT.0) THEN
         DO 200 IVER = 1,NVER
            CALL SYINI ('READ', BUFF1, DISKI, CNOI, IVER, CATOLD, LUNI,
     *         RNO, KOLS, NUMV, NUMA, NUMPOL, NUMIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN INPUT SY', IVER
               CALL MSGWRT (7)
               GO TO 200
               END IF
            NREC = BUFF1(5)
            CALL SYINI ('WRIT', BUFF2, DISKO, CNOO, IVER, CATBLK, LUNO,
     *         RNO, KOLS, NUMV, NUMA, NUMPOL, NUMIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT SY', IVER
               CALL MSGWRT (7)
               GO TO 195
               END IF
            DO 150 IREC = 1,NREC
               RNO = IREC
               CALL TABSY ('READ', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            PDIFF, PSUM, PGAIN, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ INPUT SY', IVER
                  CALL MSGWRT (7)
                  GO TO 190
                  END IF
               SWAP = .FALSE.
               DO 120 I = 1,NUMANT
                  IF (ANTS(I).EQ.ANTNO) SWAP = .TRUE.
 120              CONTINUE
               IF (SWAP) THEN
                  DO 130 I = 1,NUMIF
                     TEMP = PDIFF(1,I)
                     PDIFF(1,I) = PDIFF(2,I)
                     PDIFF(2,I) = TEMP
                     TEMP = PSUM(1,I)
                     PSUM(1,I) = PSUM(2,I)
                     PSUM(2,I) = TEMP
                     TEMP = PGAIN(1,I)
                     PGAIN(1,I) = PGAIN(2,I)
                     PGAIN(2,I) = TEMP
 130                 CONTINUE
                  END IF
               RNO = IREC
               CALL TABSY ('WRIT', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            PDIFF, PSUM, PGAIN, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ OUTPUT SY', IVER
                  CALL MSGWRT (7)
                  GO TO 190
                  END IF
 150           CONTINUE
            WRITE (MSGTXT,1050) 'SY', DISKI, CNOI, IVER, DISKO, CNOO,
     *         IVER
            CALL MSGWRT (3)
C                                       close
 190        RNO = NREC
            CALL TABSY ('CLOS', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *         PDIFF, PSUM, PGAIN, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE OUTPUT SY', IVER
               CALL MSGWRT (7)
               END IF
 195        RNO = NREC
            CALL TABSY ('READ', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *         PDIFF, PSUM, PGAIN, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE INPUT SY', IVER
               CALL MSGWRT (7)
               END IF
 200        CONTINUE
         END IF
C                                       TY tables
      CALL FNDEXT ('TY', CATOLD, NVER)
      IF (NVER.GT.0) THEN
         DO 300 IVER = 1,NVER
            CALL TYINI ('READ', BUFF1, DISKI, CNOI, IVER, CATOLD, LUNI,
     *         RNO, KOLS, NUMV, NUMPOL, NUMIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN INPUT TY', IVER
               CALL MSGWRT (7)
               GO TO 300
               END IF
            NREC = BUFF1(5)
            CALL TYINI ('WRIT', BUFF2, DISKO, CNOO, IVER, CATBLK, LUNO,
     *         RNO, KOLS, NUMV, NUMPOL, NUMIF, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'OPEN OUTPUT TY', IVER
               CALL MSGWRT (7)
               GO TO 295
               END IF
            DO 250 IREC = 1,NREC
               RNO = IREC
               CALL TABTY ('READ', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            TSYS, TANT, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ INPUT TY', IVER
                  CALL MSGWRT (7)
                  GO TO 290
                  END IF
               SWAP = .FALSE.
               DO 220 I = 1,NUMANT
                  IF (ANTS(I).EQ.ANTNO) SWAP = .TRUE.
 220              CONTINUE
               IF (SWAP) THEN
                  DO 230 I = 1,NUMIF
                     TEMP = TSYS(1,I)
                     TSYS(1,I) = TSYS(2,I)
                     TSYS(2,I) = TEMP
                     TEMP = TANT(1,I)
                     TANT(1,I) = TANT(2,I)
                     TANT(2,I) = TEMP
 230                 CONTINUE
                  END IF
               RNO = IREC
               CALL TABTY ('WRIT', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *            NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *            TSYS, TANT, IERR)
               IF (IERR.NE.0) THEN
                  WRITE (MSGTXT,1000) IERR, 'READ OUTPUT TY', IVER
                  CALL MSGWRT (7)
                  GO TO 290
                  END IF
 250           CONTINUE
            WRITE (MSGTXT,1050) 'TY', DISKI, CNOI, IVER, DISKO, CNOO,
     *         IVER
            CALL MSGWRT (3)
C                                       close
 290        RNO = NREC
            CALL TABTY ('CLOS', BUFF2, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *         TSYS, TANT, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE OUTPUT TY', IVER
               CALL MSGWRT (7)
               END IF
 295        RNO = NREC
            CALL TABTY ('READ', BUFF1, RNO, KOLS, NUMV, NUMPOL,
     *         NUMIF, TIME, TIMEI, SOURID, ANTNO, SUBA, FREQID,
     *         TSYS, TANT, IERR)
            IF (IERR.NE.0) THEN
               WRITE (MSGTXT,1000) IERR, 'CLOSE INPUT TY', IVER
               CALL MSGWRT (7)
               END IF
 300        CONTINUE
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FIXTAB ERROR:',I4,' ON ',A,' TABLE VERS',I4)
 1050 FORMAT ('Swapped ',A2,' file from vol/cno/vers',I3,I5,I4,' to',
     *   I3,I5,I4)
      END
