LOCAL INCLUDE 'FXVLA.INC'
C                                       Local include for FXVLA
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XSOUT, XDISO, APARM(10), TINT
      REAL BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER JBUFF(1024)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ,
     *   ILOCWT, CATOLD(256), INCSI, INCFI, INCIFI, INCSO,
     *   INCFO, INCIFO, LRECI, LRECO, NRPRMI, NRPRMO, ICNOIN,
     *   ILUN1, NSUBA
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, LOPCOD*4,
     *   HISCRD(10)*64
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, APARM, TINT, SEQIN, SEQOUT, DISKIN, DISKO,
     *   NUMHIS, ILOCWT, CATOLD, INCSI, INCFI, INCIFI, INCSO, INCFO,
     *   INCIFO, LRECI, LRECO, NRPRMI, NRPRMO, ICNOIN, ILUN1, NSUBA,
     *   ISCOMP
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD, LOPCOD
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFF, JBUFSZ
C                                       FQ table data
      INTEGER MXFQID
      PARAMETER (MXFQID = 12)
      DOUBLE PRECISION DFOFF(MAXIF,MXFQID)
      REAL FINC(MAXIF,MXFQID)
      INTEGER IFQID(MXFQID), NFQID
      COMMON /FQTAB/ DFOFF, FINC, IFQID, NFQID
C                                       End local include for FXVLA
LOCAL END
      PROGRAM FXVLA
C-----------------------------------------------------------------------
C! Allows user to provide subroutine to operate on UV data base
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1998, 2000-2001, 2004, 2007-2008, 2012
C;  Copyright (C) 2015
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
C   FXVLA allows a user to provide a subroutine which performs an
C   operation on a UV data base, writing an output UV data base.
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
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
C
      INTEGER ISUBA
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FXVLA.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'FXVLA '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL FXVLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      DO 100 ISUBA = 1, NSUBA
         CALL SENDUV (ISUBA, IRET)
         IF (IRET.NE.0) GO TO 990
100      CONTINUE
C
         CALL FXVHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE FXVLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   FXVLIN gets input parameters for FXVLA and creates an output file
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
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FXVLA.INC'
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
      NUMHIS = 0
      ILUN1 = 28
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 24
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
C                                       Integration time in data
      TINT = APARM(1)
C                                       Only option implemented at
C                                       present corrects leap second
C                                       clock error between 30 Dec 95
C                                       and 5 Jan 96.
      LOPCOD = '96-1'
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
      ICNOIN = OLDCNO
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
C                                       copy keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
C                                       Read FQ table into common
      CALL FQREAD (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Find number of subarrays
      CALL FNDEXT ('AN', CATOLD, NSUBA)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FXVLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('FXVLIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE SENDUV (ISUBA, IRET)
C-----------------------------------------------------------------------
C   SENDUV sends uv data one point at a time to the user supplied
C   routine and then writes the modified data if requested.
C   Inputs:
C      ISUBA   I  Subarray number.
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
      INTEGER   ISUBA, IRET
C
      CHARACTER OFILE*48, IFILE*48
      INTEGER   INIO, IPTRI, IPTRO, LUNI, LUNO, INDI, INDO, ILENBU,
     *   KBIND, NIOUT, NIOLIM, IBIND, I, IA1, IA2, INCX, BO, VO,
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY, IDUM
      LOGICAL   T, F
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FXVLA.INC'
      REAL      DUM, BASEN, CBUFF(UVBFSS), RESULT(UVBFSS)
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
C                                       Get antenna information for
C                                       this subarray.
      CALL GETANT (DISKIN, ICNOIN, ISUBA, CATOLD, BUFF1, IRET)
      IF (IRET.NE.0) THEN
         IRET = 10
         WRITE (MSGTXT,1005) ISUBA
         GO TO 990
         END IF
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
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
C                                      Call user routine.
            IF (ISCOMP) THEN
C                                       Compressed data.
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL FXVMOD (NUMVIS, BUFF1(IPTRI+ILOCU),
     *            BUFF1(IPTRI+ILOCV), BUFF1(IPTRI+ILOCW),
     *            BUFF1(IPTRI+ILOCT), IA1, IA2, ISUBA, CBUFF,
     *            BUFF1(IPTRI), INCX, RESULT, IRET)
            ELSE
C                                       Un compressed data
            CALL FXVMOD (NUMVIS, BUFF1(IPTRI+ILOCU),
     *         BUFF1(IPTRI+ILOCV), BUFF1(IPTRI+ILOCW),
     *         BUFF1(IPTRI+ILOCT), IA1, IA2, ISUBA,
     *         BUFF1(IPTRI+NRPRMI), BUFF1(IPTRI), INCX, RESULT, IRET)
            END IF
C                                       Branch on his return
            IF (IRET) 140, 130, 120
C                                       Error (fatal)
 120           WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
 130        CONTINUE
               XCOUNT = XCOUNT + 1.0D0
               CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
               IF (ISCOMP) THEN
C                                       Compressed
                  CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
               ELSE
                  CALL RCOPY (NCOPY, RESULT, BUFF2(IPTRO+NRPRMO))
                  END IF
               IPTRO = IPTRO + LRECO
               NIOUT = NIOUT + 1
C                                       OK, but no output please
 140        IPTRI = IPTRI + LRECI
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
C                                       Final call to FXVMOD.
 200     NUMVIS = -1
         CALL FXVMOD (NUMVIS, DUM, DUM, DUM, DUM, IA1, IA2, IDUM, BUFF1,
     *      BUFF1, INCX, RESULT, IRET)
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
 1000 FORMAT ('SENDUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1005 FORMAT ('SENDUV: ERROR READING AN TABLE', I5)
 1010 FORMAT ('SENDUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('SENDUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('SENDUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('SENDUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('SENDUV: FXVMOD ERROR',I3)
 1150 FORMAT ('SENDUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE FXVHIS
C-----------------------------------------------------------------------
C   FXVHIS copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FXVLA.INC'
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
C                                      Add any other history.
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
     *   FCNO(2), FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FXVHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1200 FORMAT ('FXVHIS: ERROR COPYING TABLES')
      END
      SUBROUTINE FXVMOD (NUMVIS, U, V, W, T, IA1, IA2, ISUB, VIS,
     *   RPARM, INCX, RESULT, IRET)
C-----------------------------------------------------------------------
C   Correct each VLA record individually.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      U       R    U in wavelengths
C      V       R    V in wavelengths
C      W       R    W in wavelengths
C      T       R    Time in days since 0 IAT on the first day for
C                   which there is data, the julian day corresponding
C                   to this day can be obtained in D   form by:
C                   CALL JULDAY (CATH(KHDOB),XDAY) where XDAY will
C                   be the Julian day number.
C      IA1     I    First antenna number
C      IA2     I    Second antenna number
C      ISUB    I    Subarray to process
C      RPARM   R(*) Random parameter array which includes U,V,W etc
C                   but also any other random parameters.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
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
C      U          R    U in wavelengths
C      V          R    V in wavelengths
C      W          R    W in wavelengths
C      T          R    Time in same units as input.
C      RPARM      R    Modified random parameter array.
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
      INTEGER   NUMVIS, IA1, IA2, ISUB, INCX, IRET
      REAL      U, V, W, T, VIS(INCX,*), RPARM(*), RESULT(INCX,*)
C
      LOGICAL   WGR
      DOUBLE PRECISION DFACT, DXC, DYC, DZC, DJD, DHAERR, DSHFT,
     *   DTOTFQ, DXX, DJJD, DMJD, DLONG, DLX, DLY, DLZ, DU, DV,
     *   DW, DTUVW, DTUT1, DCOSD, DSIND, DCOSHA, DSINHA, DRAX,
     *   DECX, DRHO, DGEO, DLST, DELDAT, DCROTP, DSROTP, DFUV,
     *   DGASTM, DTC, DEPS, DELPSI, DELEPS, DSPSI, DSEPS, DEQEQ,
     *   DROTP, DSRA, DSDEC, DRAY, DECY, DHA, OBSPOS(3)
      COMPLEX ZVIS, ZROT
      REAL TERR, UTCIAT, TU1UTC, POLAR(2)
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, ISOU,
     *   IBASE, IARRAY, J, IFQD, IFQENT, WDIR
      SAVE DJD, DMJD, DLONG, DGASTM, DFUV
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FXVLA.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DSOU.INC'
      INCLUDE 'INCS:DANT.INC'
      INCLUDE 'INCS:DANS.INC'
      DATA DELDAT / 0.05D0/
      DATA WDIR, WGR /1, .TRUE./
      DATA POLAR /2 * 0.0/
C-----------------------------------------------------------------------
      IRET = 0
C                                       Initialize earth rotation/time
C                                       parameters for this subarray
      IF (NUMVIS.EQ.1) THEN
C                                       Check for missing/inconsistent
C                                       AN table parameters:
         IF (ANAME.NE.'VLA') THEN
            IRET = 1
            WRITE (MSGTXT,1010)
            GO TO 990
            END IF
C                                       MJD of reference date
         CALL JULDAY (RDATE, DJD)
         DMJD = DJD - 2400000.5D0
C                                       Array center longitude
         DLONG = ATAN2 (-ARRAYC(2), ARRAYC(1))
C                                       Equation of equinoxes
         DTC = (DJD - 2433282.423D0) / 36524.21988D0
         DEPS = -46.850D0 * DTC - 0.0034D0 * DTC * DTC +
     *      0.0018D0 * DTC * DTC * DTC
         DEPS = (84404.84D0 + DEPS) / 3600.0D0 * DG2RAD
         CALL NUT4 (DJD, DELPSI, DELEPS, DSPSI, DSEPS)
         DEQEQ = DELPSI * COS (DEPS)
C                                       Position for diurnal
C                                       aberration
         DRHO = SQRT (ARRAYC(1)**2 + ARRAYC(2)** 2 + ARRAYC(3)**2)
         OBSPOS(3) = DRHO
         DRHO = DRHO / 6378140.0D0

         DGEO = SQRT (ARRAYC(1)**2 + ARRAYC(2)**2)
         DGEO = ATAN2 (ARRAYC(3), DGEO)
         OBSPOS(1) = DGEO
C                                       East longtitude
         OBSPOS(2) = - DLONG
C                                       GAST at UT midnight
         DGASTM = GSTIAT + DEQEQ
C                                       (u,v,w) scaling factor
         DFUV = SAFREQ / VELITE
         END IF
C
      IF (NUMVIS.GT.0) THEN
C                                       Get data pointers
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
C                                       Case option of:
C                                       1: '96-1' - leap sec.
C                                          error Dec95/Jan96.
         IF (LOPCOD.NE.'96-1') GO TO 200
C                                       Is source no. current ?
            ISOU = 1
            IF (ILOCSU.GE.0) ISOU = RPARM(1+ILOCSU)
            IF ((NUMVIS.EQ.1).OR.(ISOU.NE.IDSOUR)) THEN
C                                       Read source data
               CALL GETSOU (ISOU, DISKIN, ICNOIN, CATOLD, ILUN1, IRET)
               IF (IRET.NE.0) GO TO 999
               END IF
C                                       Get subarray number
            IF (ILOCB.GE.0) THEN
               IBASE = RPARM(1+ILOCB)
               IARRAY = 100.0 * (RPARM(1+ILOCB) - IBASE) + 1.5
            ELSE
               IARRAY = RPARM(1+ILOCSA)
               END IF
C                                       Process only data for the
C                                       selected subarray
            IF (IARRAY.NE.ISUB) THEN
               CALL RCOPY (NIF*NF*NS*INCX, VIS, RESULT)
               GO TO 999
               END IF
C                                       Get freqid.
            IFQD = 1
            IF (ILOCFQ.GE.0) IFQD = RPARM(1+ILOCFQ)
C                                       Search FQ table
            IFQENT = 0
            DO 80 J = 1, NFQID
               IF (IFQID(J).EQ.IFQD) IFQENT = J
80             CONTINUE
            IF (IFQENT.EQ.0) THEN
               IRET = 1
               WRITE (MSGTXT,1080) IFQD
               GO TO 990
               END IF
C
            CALL CLK96 (DJD, T, TERR, UTCIAT, TU1UTC)
C                                       Convert IAT to UT1 (include
C                                       known VLA clock error)
            DTUVW = T - (TINT / 2.0 - TERR) / 86400.0D0
            DTUT1 = DTUVW + (TU1UTC + UTCIAT) / 86400.0D0
C                                       Calculate LST
            DLST = DGASTM + DTUVW * ROTIAT
C                                       Precess to date to obtain
C                                       apparent position that would
C                                       have been obtained without
C                                       clock error.
            DJJD = DJD + DTUT1
C                                       JPRECS works for both J2000 and
C                                       B1950 starting December 2004
            CALL JPRECS (DJJD, EPOCH, DELDAT, WDIR, WGR, OBSPOS, POLAR,
     *         RAEPO, DECEPO, DRAX, DECX)
C                                       Precess with clock error
C                                       to obtain apparent position
C                                       used by the VLA.
            DJJD = DJJD - TERR / 86400.0D0
C
            CALL JPRECS (DJJD, EPOCH, DELDAT, WDIR, WGR, OBSPOS, POLAR,
     *         RAEPO, DECEPO, DRAY, DECY)
C                                       Convert clock error to
C                                       a position offset.
            DHAERR = -TERR / 86400.0 * ROTIAT + (DRAY - DRAX)
            DXC = COS (DECY) * SIN (DHAERR)
            DYC = -COS (DECY) * SIN (DECX) * COS (DHAERR) +
     *         COS (DECX) * SIN (DECY)
            DZC = TWOPI * (SQRT (1.0D0 - DXC*DXC - DYC*DYC) - 1.0D0)
            DXC = TWOPI * DXC
            DYC = TWOPI * DYC
C                                       Shift factor
            DSHFT = U * DXC + V * DYC + W * DZC
C
            DO 140 JIF = 1,NIF
               DO 130 JF = 1,NF
                  DO 120 JS = 1,NS
                     INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *                  (JS-1) * INCSI + 1
                     INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *                  (JS-1) * INCSO + 1
C                                       Calculate true freq. scaling
                     DTOTFQ = FREQ + (JF - 1) * FINC(JIF,IFQENT) +
     *                  DFOFF(JIF,IFQENT)
                     DFACT = DTOTFQ / SAFREQ
C                                       Rotate phase for position
C                                       offset
                     DXX = DFACT * DSHFT
                     ZROT = CMPLX (COS (DXX), -SIN (DXX))
                     ZVIS = CMPLX (VIS(1,INDEXI), VIS(2,INDEXI))
                     ZVIS = ZVIS * ZROT
                     RESULT(1,INDEXO) = REAL (ZVIS)
                     RESULT(2,INDEXO) = AIMAG (ZVIS)
                     RESULT(3,INDEXO) = VIS(3,INDEXI)
 120                 CONTINUE
 130              CONTINUE
 140           CONTINUE
C                                       Recompute (u,v,w)
C                                       Compute hour angle
            DHA = DGASTM - DLONG + DTUT1 * ROTIAT - DRAX
            DCOSD = COS (DECX)
            DSIND = SIN (DECX)
            DCOSHA = COS (DHA)
            DSINHA = SIN (DHA)
            DLX = (STNX(IA1) - STNX(IA2)) * DFUV
            DLY = (STNY(IA1) - STNY(IA2)) * DFUV
            DLZ = (STNZ(IA1) - STNZ(IA2)) * DFUV
            DU = DLX * DSINHA + DLY * DCOSHA
            DV = DSIND * (-DLX * DCOSHA + DLY * DSINHA) + DLZ * DCOSD
            DW = DCOSD * (DLX * DCOSHA - DLY * DSINHA) + DLZ * DSIND
C                                       Coefficients for rotation
C                                       of (u,v,w) to epoch. Precess
C                                       two nearby positions.
C            DLST = 0.0D0
C
            CALL JPRECS (DJD, EPOCH, DELDAT, WDIR, WGR, OBSPOS, POLAR,
     *         RAEPO, DECEPO, DRAX, DECX)
C
            DSRA = RAEPO
            DSDEC = DECEPO + (10.0 / 3600.0) * DG2RAD
            CALL JPRECS (DJD, EPOCH, DELDAT, WDIR, WGR, OBSPOS, POLAR,
     *         DSRA, DSDEC, DRAY, DECY)
            DROTP = -ATAN2 ((DRAY-DRAX)*COS(DECEPO), DECY-DECX)
            DCROTP = COS (DROTP)
            DSROTP = SIN (DROTP)
C                                       Rotate to orientation of epoch
            U = DU * DCROTP + DV * DSROTP
            V = DV * DCROTP - DU * DSROTP
            W = DW
            GO TO 700
C                                       2: Other
200      CONTINUE
C                                       Endcase
700      CONTINUE
C
         END IF
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C
999   RETURN
C----------------------------------------------------------------------
1010  FORMAT ('THESE DATA ARE NOT FROM THE VLA; CHECK AN TABLE')
1080  FORMAT ('MISSING FQID ',I3,' IN FQ TABLE')
      END
      SUBROUTINE FQREAD (IRET)
C----------------------------------------------------------------------
C   Subroutine to read parts of the FQ table into memory
C   Outputs:
C      IRET      I      Return code; (0 => ok)
C----------------------------------------------------------------------
      INTEGER IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'FXVLA.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DMSG.INC'
      REAL      TOTBW(MAXIF)
      INTEGER   FQKOLS(MAXFQC), FQNUMV(MAXFQC)
      INTEGER   ISBAND(MAXIF), NIF, I, J, IVER, IFQRNO, IERR
      CHARACTER BNDCOD(MAXIF)*8
C---------------------------------------------------------------------
C                                       Initialization
      IRET = 0
C                                       Find no. of FQ_ID's
      IVER = 1
      CALL FQINI ('READ', JBUFF, DISKIN, ICNOIN, IVER, CATOLD, ILUN1,
     *   I, FQKOLS, FQNUMV, NIF, IRET)
      IF (IRET.NE.0) GO TO 999
      NFQID = JBUFF(5)
C                                       Exceeds maximum ?
      IF (NFQID.GT.MXFQID) THEN
         IRET = 10
         WRITE (MSGTXT,1020)
         GO TO 990
         END IF
C                                       Read FQ table into memory
      DO 100 I = 1, NFQID
         IFQRNO = I
         CALL TABFQ ('READ', JBUFF, IFQRNO, FQKOLS, FQNUMV, NIF,
     *      IFQID(I), DFOFF(1,I), FINC(1,I), TOTBW, ISBAND, BNDCOD,
     *      IERR)
         IF (IERR.NE.0) THEN
            IRET = 1
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
C                                       Correct for sideband
         DO 80 J = 1, NIF
            IF (ISBAND(J).LT.0) FINC(J,I) = -ABS (FINC(J,I))
80          CONTINUE
100      CONTINUE
C                                       Close FQ table
      CALL TABIO ('CLOS', 0, I, JBUFF, JBUFF, IRET)
      IF (IRET.NE.0) GO TO 999
      GO TO 999
C                                       Error
990   CALL MSGWRT (8)
C                                       Exit
999   RETURN
C----------------------------------------------------------------------
1020  FORMAT ('FQREAD: PARAMETER MXFQID TOO SMALL; CONTACT AIPS ADMIN')
1100  FORMAT ('FQREAD: ERR',I3,' READING FQ/CH TABLE')
      END
      SUBROUTINE CLK96 (DJD, T, TERR, UTCIAT, TU1UTC)
C----------------------------------------------------------------------
C   Compute the UTC clock error for leap sec. err. 30 Dec 95-5 Jan 96
C   Input:
C      DJD      D      Modified Julian date (ref. date)
C      T        R      Time wrt DJD
C   Output:
C      TERR     R      Difference to be added to VLA claimed UTC
C                      to yield true UTC.
C      UTCIAT   R      UTC - IAT (seconds)
C      TU1UTC   R      UT1 - UTC (seconds)
C----------------------------------------------------------------------
      DOUBLE PRECISION DJD
      REAL T, TERR, UTCIAT, TU1UTC
C
      DOUBLE PRECISION DERROR(7), DTFRAC
      REAL TABIAT(7), TABUT1(7)
      INTEGER I, IT
C                                       Values start at MJD 50081
C                                       (= Dec 30, 1995)
      DATA DERROR /-0.1438, -0.2943, -0.4442, 0.4057, 0.2555, 0.1054,
     *   -0.0448/
      DATA TABUT1 /-0.4407, -0.4431, 0.5548, 0.5527, 0.5507, 0.5487,
     *    0.5468/
      DATA TABIAT /-29.0, -29.0, -30.0, -30.0, -30.0, -30.0, -30.0/
C----------------------------------------------------------------------
C                                       Intialization
      TERR = 0.0
      UTCIAT = 0.0
      TU1UTC = 0.0
C
      IT = T
      DTFRAC = T - IT
      I = (DJD - 2450080.5D0) + IT
      IF ((I.GE.1).AND.(I.LE.8)) THEN
C                                       UTC - IAT
         UTCIAT = TABIAT(I)
C                                       UT1 - UTC
         TU1UTC = TABUT1(I)
C                                       Error in VLA UTC
         DTFRAC = DTFRAC + UTCIAT / 86400.0
         TERR = DERROR(I) - 0.006265 * DTFRAC * 24.0
         END IF
C
      RETURN
      END
