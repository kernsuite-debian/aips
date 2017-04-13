LOCAL INCLUDE 'UVPOL.INC'
C                                       Local include for UVPOL
C      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XOPCO
      REAL      XSIN, XDISIN, XSOUT, XDISO, BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, OPCODE*4, NAMOUT*12, CLAOUT*6,
     *   HISCRD(10)*64
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XOPCO,
     *   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, ILOCWT, CATOLD, INCSI,
     *   INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI, LRECO, NRPRMI,
     *   NRPRMO, ISCOMP
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, OPCODE, HISCRD
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
LOCAL END
      PROGRAM UVPOL
C-----------------------------------------------------------------------
C! Modifies a data set to allow forming a complex Stokes image and beam
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1996, 1999-2000, 2008, 2015
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
C   Modifies a data set to allow forming a complex Stokes image and beam
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
C      OPCODE         OPCODE        Operation memnonic
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'UVPOL.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVPOL '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVPOIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Modify data
      CALL CMPXUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVPHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVPOIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVPOIN gets input parameters for UVPOL and creates an output file
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
C   See prologue comments in UVPOL for more details.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX
      LOGICAL   T
      INCLUDE 'UVPOL.INC'
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
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 15
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
      CALL H2CHR (4, 1, XOPCO, OPCODE)
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
 1000 FORMAT ('UVPOIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('UVPOIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE CMPXUV (IRET)
C-----------------------------------------------------------------------
C   CMPXUV sends uv data one point at a time to thedata modification
C   routine.
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
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY
      LOGICAL   T, F
      INCLUDE 'UVPOL.INC'
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
            ELSE
               IA1 = BUFF1(IPTRI+ILOCA1) + 0.1
               IA2 = BUFF1(IPTRI+ILOCA2) + 0.1
               END IF
            NUMVIS = NUMVIS + 1
C                                      Call modification routine.
            IF (ISCOMP) THEN
C                                       Compressed data.
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               CALL DOCMPX (NUMVIS, CBUFF, INCX, RESULT, IRET)
            ELSE
C                                       Un compressed data
               CALL DOCMPX (NUMVIS, BUFF1(IPTRI+NRPRMI), INCX, RESULT,
     *            IRET)
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
C                                       Final call to DOCMPX.
 200     NUMVIS = -1
         CALL DOCMPX (NUMVIS, BUFF1, INCX, RESULT, IRET)
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
 1000 FORMAT ('CMPXUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('CMPXUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('CMPXUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('CMPXUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('CMPXUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('CMPXUV: DOCMPX ERROR',I3)
 1150 FORMAT ('CMPXUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE UVPHIS
C-----------------------------------------------------------------------
C   UVPHIS copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'UVPOL.INC'
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
C                                       OPCODE
      WRITE (HILINE,2040) TSKNAM, OPCODE
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
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
 1000 FORMAT ('UVPHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1200 FORMAT ('UVPHIS: ERROR COPYING TABLES')
 2040 FORMAT (A6,' OPCODE =''',A,''' / Operation')
      END
      SUBROUTINE DOCMPX (NUMVIS, VIS, INCX, RESULT, IRET)
C-----------------------------------------------------------------------
C   Actually modifies data.
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      OPCODE     C*4     Opcode
C      LRECI      I    Input file record length
C      NRPRMI     I    Input number of random parameters.
C   Output:
C      RESULT  R(INCX,*) Output visibilities selected in frequency.
C      IRET       I    Return code  -1 => don't write
C                                    0 => OK
C                                   >0 => error, terminate.
C
C   Output in COMMON:
C      NUMHIS    I         # history entries (max. 10)
C      HISCRD    C(NUMHIS) History records
C-----------------------------------------------------------------------
      INTEGER   NUMVIS, INCX, IRET
      REAL      VIS(INCX,*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, WTCNT, IROUND
      REAL      WTSUM, WTAVG, S
      LOGICAL   DODATA, ISCROS(4), DOIT
      INCLUDE 'UVPOL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE DODATA, NS, NIF, NF, ISCROS
C-----------------------------------------------------------------------
      IRET = 0
C                                       Setup first call
      IF (NUMVIS.EQ.1) THEN
C                                       Modify data?
         DODATA = OPCODE.EQ.'DATA'
C                                       How much of what?
         NS = 1
         ISCROS(1) = .FALSE.
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) THEN
            NS = CATBLK(KINAX+JLOCS)
            DO 5 JS = 1,NS
               S = CATD(KDCRV+JLOCS) + (JS - CATR(KRCRP+JLOCS)) *
     *            CATR(KRCIC+JLOCS)
               JF = IROUND (S)
               ISCROS(JS) = (JF.EQ.-3) .OR. (JF.EQ.-4) .OR. (JF.LT.-6)
     *            .OR. (JF.EQ.2) .OR. (JF.EQ.3)
 5             CONTINUE
            END IF
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         END IF
C                                       Loop over data
      IF (NUMVIS.GT.0) THEN
         IRET = -1
         DO 40 JIF = 1,NIF
            DO 30 JF = 1,NF
C                                        Find average Stokes' weight
               WTSUM = 0.0
               WTAVG = 0.0
               WTCNT = 0
               DOIT = .FALSE.
               DO 10 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
                  IF (VIS(3,INDEXI).GT.0.0) THEN
                     WTSUM = WTSUM + VIS(3,INDEXI)
                     WTCNT = WTCNT + 1
                     IF (ISCROS(JS)) DOIT = .TRUE.
                     END IF
 10               CONTINUE
               IF (DOIT) THEN
                  WTAVG = WTSUM / WTCNT
                  IRET = 0
                  END IF
               DO 20 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
C                                       Zero amplitude data is assumed
C                                       invalid.
                  IF ((ABS (VIS(1,INDEXI)) + ABS (VIS(1,INDEXI))) .LE.
     *               1.0E-20) VIS(3,INDEXI) = -1.0
                  IF (VIS(3,INDEXI).GT.0.0) THEN
C                                       Replace valid data with (1,0)
                     IF (DODATA) THEN
                        RESULT(1,INDEXO) = 1.0
                        RESULT(2,INDEXO) = 0.0
C                                       Pass original data.
                     ELSE
                        RESULT(1,INDEXO) = VIS(1,INDEXI)
                        RESULT(2,INDEXO) = VIS(2,INDEXI)
                        END IF
C                                       Zero invalid data
                  ELSE
                     RESULT(1,INDEXO) = 0.0
                     RESULT(2,INDEXO) = 0.0
                     END IF
C                                       Replace weight with average.
                  RESULT(3,INDEXO) = WTAVG
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
         END IF
C
 999  RETURN
      END
