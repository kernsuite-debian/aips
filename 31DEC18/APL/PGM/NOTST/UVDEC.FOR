LOCAL INCLUDE 'UVDEC.INC'
C                                       Local include for UVDEC
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XSOUT, XDISO, XBCH, XECH, XCHINC,
     *   BUFF1(UVBFSS), BUFF2(UVBFSS)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, BCHAN, ECHAN, CHINC,
     *   NUMHIS, JBUFSZ, ILOCWT, CATOLD(256), INCSI, INCFI, INCIFI,
     *   INCSO, INCFO, INCIFO, LRECI, LRECO, NRPRMI, NRPRMO,
     *   IBUFF1(UVBFSS), IBUFF2(UVBFSS)
      LOGICAL   ISCOMP
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, HISCRD(10)*64
      EQUIVALENCE (IBUFF1, BUFF1), (IBUFF2, BUFF2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XBCH, XECH, XCHINC,
     *   SEQIN, SEQOUT, DISKIN, DISKO, BCHAN, ECHAN, CHINC, NUMHIS,
     *   ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, ISCOMP
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD
      COMMON /BUFRS/ CATOLD, BUFF1, BUFF2, JBUFSZ
C                                       End local include for UVDEC
LOCAL END
      PROGRAM UVDEC
C-----------------------------------------------------------------------
C! Copies every n'th channel
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 2002, 2007-2010, 2012, 2015
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
C   UVDEC copies every n'th spectral channel.
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
C      BCHAN          BCHAN         Begin channel
C      ECHAN          ECHAN         End channel
C      CHINC          CHINC         Channel increment
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER   IRET
      INCLUDE 'UVDEC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'UVDEC '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL UVDECI (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL UVDECU (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL UVDECH
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE UVDECI (PRGN, JERR)
C-----------------------------------------------------------------------
C   UVDECI gets input parameters for UVDEC and creates an output file
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
      CHARACTER STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX
      LOGICAL   T
      INCLUDE 'UVDEC.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA T /.TRUE./, BLANK / ' '/
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
      NPARM = 17
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
C                                       Rest of adverbs
      BCHAN = IROUND (XBCH)
      ECHAN = IROUND (XECH)
      CHINC = IROUND (XCHINC)
      IF ((BCHAN.LT.1) .OR. (BCHAN.GT.CATBLK(KINAX+JLOCF))) BCHAN = 1
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
      ECHAN = MIN (CATBLK(KINAX+JLOCF), ECHAN)
      IF (CHINC.LT.1) CHINC = 1
      ECHAN = BCHAN + CHINC * ((ECHAN-BCHAN)/CHINC)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       Fix header
      CATBLK(KINAX+JLOCF) = (ECHAN - BCHAN) / CHINC + 1
      CATR(KRCIC+JLOCF) = CATR(KRCIC+JLOCF) * CHINC
      CATR(KRCRP+JLOCF) = (CATR(KRCRP+JLOCF) - BCHAN) / CHINC + 1.0
      CATR(KRARP) = (CATR(KRARP) - BCHAN) / CHINC + 1.0
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
C                                       copy header keywords
      CALL KEYCOP (DISKIN, OLDCNO, DISKO, CCNO, IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVDECI: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('UVDECI: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE UVDECU (IRET)
C-----------------------------------------------------------------------
C   UVDECU sends uv data one point at a time to the user supplied
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
     *   NUMVIS, XCOUNT, NCORI, NCORO, NCOPY
      LOGICAL   T, F
      INCLUDE 'UVDEC.INC'
      REAL      CBUFF(UVBFSS), RESULT(UVBFSS)
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
               IA2 = BUFF1(IPTRI+ILOCB) + 0.1
               IA1 = IA2 / 256
               IA2 = IA2 - IA1*256
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
               CALL DIDDLE (NUMVIS, CBUFF, INCX, RESULT, IRET)
            ELSE
C                                       Un compressed data
               CALL DIDDLE (NUMVIS, BUFF1(IPTRI+NRPRMI), INCX, RESULT,
     *            IRET)
               END IF
C                                       Branch on his return
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1.0D0
               CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
C                                       Compressed
               IF (ISCOMP) THEN
                  CALL ZUVPAK (NCORO, RESULT, BUFF2(IPTRO+ILOCWT),
     *               BUFF2(IPTRO+NRPRMO))
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
C                                       Final call to DIDDLE.
 200  NUMVIS = -1
      CALL DIDDLE (NUMVIS, BUFF1, INCX, RESULT, IRET)
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
 1000 FORMAT ('UVDECU: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('UVDECU: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('UVDECU: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('UVDECU: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('UVDECU: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('UVDECU: DIDDLE ERROR',I3)
 1150 FORMAT ('UVDECU: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE UVDECH
C-----------------------------------------------------------------------
C   UVDECH copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      INTEGER   NONOT
      PARAMETER (NONOT=6)
C
      CHARACTER NOTTYP(NONOT)*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I
      LOGICAL   T
      INCLUDE 'UVDEC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NOTTYP /'BP', 'FG', 'GC', 'FQ', 'CP', 'PD'/
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
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
      CALL COPTAB (IERR)
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('UVDECH: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' /')
 1200 FORMAT ('UVDECH: ERROR COPYING TABLES')
      END
      SUBROUTINE COPTAB (IRET)
C-----------------------------------------------------------------------
C   Updates tables for selection by IF
C   Inputs in common:
C      BIF   I  First IF
C      EIF   I  Highest IF selected
C      FQOFF D  Frequency offset
C      SELIF L  Select IFs or not
C   Output:
C      IRET  I  Return code, 0=>OK
C-----------------------------------------------------------------------
      INTEGER   IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IERR, LUN1, LUN2, VER, NVER, OFQID, ISUB, JSUB, BIF,
     *   EIF, AN(1), NA, ISBAND(MAXIF), TNIF, IBUFF(512), IFQ, OFQ,
     *   IFQRNO, FQKOLS(MAXFQC), FQNUMV(MAXFQC), J
      LOGICAL   TABLE, EXIST, FITASC
      DOUBLE PRECISION TB, TE, FOFF(MAXIF)
      REAL      FINC(MAXIF)
      CHARACTER BNDCOD(MAXIF)*8
      INCLUDE 'UVDEC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /28,29/
C-----------------------------------------------------------------------
      MSGTXT = 'Updating tables for channel selection'
      CALL MSGWRT (4)
      ISUB = 0
      JSUB = -1
C                                       Revise tables: note depends on
C                                       doing loops 0 times if none
      OFQID = 0
      BIF = 1
      EIF = CATBLK(KINAX+JLOCIF)
      TB = -1.D8
      TE = 1.D8
      NA = 0
      AN(1) = 0
C                                       BP tables
      CALL FNDEXT ('BP', CATOLD, NVER)
      DO 110 VER = 1,NVER
         CALL ISTAB ('BP', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL BPFSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF,
     *      BCHAN, ECHAN, CHINC, TB, TE, OFQID, ISUB, JSUB, IBUFF1,
     *      IBUFF2, IRET)
 110     CONTINUE
C                                       FG tables
      CALL FNDEXT ('FG', CATOLD, NVER)
      DO 120 VER = 1,NVER
         CALL ISTAB ('FG', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL FGSELD (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF,
     *      BCHAN, ECHAN, CHINC, TB, TE, OFQID, ISUB, JSUB, IBUFF1,
     *      IBUFF2, IRET)
 120     CONTINUE
C                                       GC tables
      CALL FNDEXT ('GC', CATOLD, NVER)
      DO 130 VER = 1,NVER
         CALL ISTAB ('GC', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL GCSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BCHAN,
     *      ECHAN, BIF, EIF, OFQID, AN, NA, ISUB, JSUB, IBUFF1, IBUFF2,
     *      IRET)
 130     CONTINUE
C                                       FQ table
      VER = 1
      CALL ISTAB ('FQ', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE, EXIST,
     *   FITASC, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1130) IERR, 'ISTAB'
         GO TO 990
         END IF
C                                       Find number of FQ entries
      IF (EXIST) THEN
         CALL FQINI ('READ', IBUFF, DISKIN, FCNO(2), VER, CATOLD,
     *      LUN1, IFQRNO, FQKOLS, FQNUMV, TNIF, IERR)
         IF (IERR.EQ.0) THEN
            CALL TABIO ('CLOS', 0, IFQRNO, IBUFF, IBUFF, IERR)
            IF (IERR.GT.0) GO TO 999
            NVER = IBUFF(5)
         ELSE
            WRITE (MSGTXT,1130) IERR, 'FQINI'
            GO TO 990
            END IF
C                                       This will do for CH table
C                                       or no table cases
      ELSE
         NVER = 1
         END IF
C                                       Loop over all desired FREQids
      DO 150 IFQ = 1,NVER
         CALL CHNDAT ('READ', IBUFF, DISKIN, FCNO(2), VER, CATOLD,
     *      LUN1, TNIF, FOFF, ISBAND, FINC, BNDCOD, IFQ, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1130) IERR, 'READ FQ'
            GO TO 990
            END IF
         DO 140 J = BIF, EIF
            FINC(J) = FINC(J) * CHINC
 140        CONTINUE
C                                       Write new FQ table
         OFQ = IFQ
         CALL CHNDAT ('WRIT', IBUFF, DISKO, FCNO(1), VER, CATBLK, LUN1,
     *      TNIF, FOFF, ISBAND, FINC, BNDCOD, OFQ, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1130) IERR, 'WRIT FQ'
            GO TO 990
            END IF
 150     CONTINUE
C                                       CP tables
      CALL FNDEXT ('CP', CATOLD, NVER)
      DO 160 VER = 1,NVER
         CALL ISTAB ('CP', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST.AND.(IERR.EQ.0)) CALL CPFSEL (DISKIN, FCNO(2), DISKO,
     *      FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF, BCHAN,
     *      ECHAN, CHINC, OFQID, IBUFF1, IBUFF2, IRET)
         IF (IRET.GT.0) GO TO 999
 160     CONTINUE
C                                       PD tables
      CALL FNDEXT ('PD', CATOLD, NVER)
      DO 170 VER = 1,NVER
         CALL ISTAB ('PD', DISKIN, FCNO(2), VER, LUN1, BUFF1, TABLE,
     *      EXIST, FITASC, IERR)
         IF (EXIST .AND. (IERR.EQ.0)) CALL PDFSEL (DISKIN, FCNO(2),
     *      DISKO, FCNO(1), VER, CATOLD, CATBLK, LUN1, LUN2, BIF, EIF,
     *      BCHAN, ECHAN, CHINC, OFQID, ISUB, JSUB, IBUFF1, IBUFF2,
     *      IRET)
 170     CONTINUE
      GO TO 999
C
 990  IRET = IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1130 FORMAT ('ERROR',I5,' FQ TABLE OPER = ',A)
      END
      SUBROUTINE BPFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, CHINC, TB, TE, IFQID, ISUB,
     *   JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies and renumbers the IFs and channels in an BP table; can also
C   modify the FQ ID.
C   Inputs:
C      DISKI           I       Input volume number
C      CNOI            I       Input catalog number
C      DISKO           I       Output volume number
C      CNOO            I       Output catalog number
C      VER             I       Version to check/modify
C      CATIN(256)      I       Input catalog header
C      CATOUT(256)     I       Output catalog header
C      LUNI            I       LUN to use
C      LUNO            I       LUN to use
C      BIF             I       Start IF number
C      EIF             I       End IF number
C      BCHAN           I       First channel
C      ECHAN           I       Last channel
C      CHINC           I       Channel increment
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      IFQID           I       FQ ID to select (output FQID will be 1)
C                              if <= 0 then output FQ ID unchanged.
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER          I(512)  Work buffer
C      OBUFF           I(512)  Work buffer
C   Output:
C      IRET            I       Error, 0 => OK
C   Changed version to do CHINC
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, CHINC, IFQID, ISUB, JSUB,
     *   BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      DOUBLE PRECISION IFFREQ(MAXIF), TIME
      REAL      BNDPAS(2,MAXCIF), WEIGHT(2*MAXIF), INTERV, BANDW,
     *   LOWSHF, DELSHF
      INTEGER   IBPRNO, BPKOLS(MAXBPC), BPNUMV(MAXBPC), OKOLS(MAXBPC),
     *   ONUMV(MAXBPC), NBPROW, OBPRNO, SOURID, SUBA, FREQID, REFANT(2),
     *   NUMANT, NUMPOL, NUMIF, NUMFRQ, JCHAN, ANT, I, J, K, L, JJ,
     *   ICHAN, NEWIF, NEWFRQ, IFA, IFB, IFREQA, IFREQB, NUMSHF, INX,
     *   LNX, OVER
      LOGICAL   REFMT
      CHARACTER LBPKEY*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open BP file
      CALL BPINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, IBPRNO,
     *   BPKOLS, BPNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, JCHAN, NUMSHF,
     *   LOWSHF, DELSHF, LBPKEY, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       If polynomial bandpass then
C                                       don't renumber channels.
C                                       1: Conventional BP.
C                                       New BP table will cover
C                                       freq. [IFREQA,IFREQB]
      REFMT = .FALSE.
      IF (LBPKEY.EQ.' ') THEN
         IFREQA = MAX (BCHAN, JCHAN)
         IFREQB = MIN (JCHAN+NUMFRQ-1, ECHAN)
         NEWFRQ = (IFREQB - IFREQA) / CHINC + 1
C                                       Start. chan. in new BP table
C                                       using new chan. numbering.
         ICHAN = IFREQA - BCHAN + 1
C                                       2: Polynomial BP.
C                                       No renumbering.
      ELSE
         IFREQA = JCHAN
         NEWFRQ = NUMFRQ
         ICHAN = JCHAN - BCHAN + 1
         END IF
C                                       New BP table will cover
C                                       IF range [IFA,IFB].
      IFA = BIF
      IFB = MIN (NUMIF, EIF)
      NEWIF = IFB - IFA + 1
C                                       # rows in old table
      NBPROW = BUFFER(5)
C                                       Open up new BP table
      OVER = VER
      CALL BPINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OBPRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NEWIF, NEWFRQ, ICHAN,
     *   NUMSHF, LOWSHF, DELSHF, LBPKEY, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 300 I = 1,NBPROW
         CALL TABBP ('READ', BUFFER, IBPRNO, BPKOLS, BPNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, BANDW,
     *      IFFREQ, FREQID, REFANT, WEIGHT, BNDPAS, IRET)
C                                       Check return code
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
C                                       Is this FQ ID selected ?
         IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND. (IFQID.NE.FREQID))
     *      IRET = -10
         IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *      IRET = -10
         IF ((TIME.LT.TB) .OR. (TIME.GT.TE)) IRET = -10
C                                       suppress
C                                       IRET=-3 means all sol'n failed
C                                       must keep to leave DOBAND
C                                       unchanged
         IF ((IRET.LT.-3) .OR. (NEWIF.LE.0) .OR. (NEWFRQ.LE.0)) THEN
            REFMT = .TRUE.
C                                       Copy this record
         ELSE
            DO 40 L = 1,NUMPOL
               DO 30 J = 1,NEWIF
                  JJ = BIF + J - 1
                  IF (L.EQ.1) IFFREQ(J) = IFFREQ(JJ)
                  WEIGHT(J+(L-1)*NEWIF) = WEIGHT(JJ+(L-1)*NUMIF)
                  INX = (J - 1) * NEWFRQ + (L - 1) * NEWFRQ * NEWIF
                  LNX = (JJ - 1) * NUMFRQ + (L - 1) * NUMFRQ * NUMIF +
     *               (IFREQA - JCHAN) - CHINC + 1
                  IF (INX.NE.LNX) REFMT = .TRUE.
C                                       Renumber IFs/FREQ's
                  DO 20 K = 1,NEWFRQ
                     INX = INX + 1
                     LNX = LNX + CHINC
                     BNDPAS(1,INX) = BNDPAS(1,LNX)
                     BNDPAS(2,INX) = BNDPAS(2,LNX)
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
C                                       Set output FQ ID
            IF (IFQID.GT.0) FREQID = 1
            IF (JSUB.GE.0) SUBA = JSUB
C                                       Write output record.
            CALL TABBP ('WRIT', OBUFF, OBPRNO, OKOLS, ONUMV, NEWIF,
     *         NEWFRQ, NUMPOL, TIME, INTERV, SOURID, SUBA, ANT, BANDW,
     *         IFFREQ, FREQID, REFANT, WEIGHT, BNDPAS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
            END IF
 300     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IBPRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OBPRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1300) 'Reformatted BP', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1300) 'Copied BP', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('BPFSEL: ERROR ',I3,' RETURNED FROM BPINI')
 1020 FORMAT ('BPFSEL: ERROR ',I3,' RETURNED FROM TABBP')
 1300 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
      SUBROUTINE CPFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, CHINC, IFQID, BUFFER,
     *   OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies and renumbers the IFs and channels in an CP table; can also
C   modify the FQ ID.  version with channel increment
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      CATOUT   I(256)   Output catalog header
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      BCHAN    I        First channel
C      ECHAN    I        Last channel
C      IFQID    I        FQ ID to select (output FQID will be 1)
C                          if <= 0 then output FQ ID unchanged.
C   Input/Output:
C      BUFFER   I(512)   Work buffer
C      OBUFF    I(512)   Work buffer
C   Output:
C      IRET     I        Error, 0 => OK, -1 => okay but no copy
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, CHINC, IFQID, BUFFER(*),
     *   OBUFF(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXCPC
      PARAMETER (MAXCPC = 6)
C
      REAL      CFLUX(4,MAXCIF)
      INTEGER   ICPRNO, CPKOLS(MAXCPC), CPNUMV(MAXCPC), OKOLS(MAXCPC),
     *   ONUMV(MAXCPC), NCPROW, OCPRNO, FREQID, NUMIF, NUMFRQ, I, J, K,
     *   JJ, ICHAN, NEWIF, NEWFRQ, IFA, IFB, IFREQA, IFREQB, INX, LNX,
     *   OVER, SUID, FOPEN
      LOGICAL   REFMT
      CHARACTER SOURCE*16
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      REFMT = .FALSE.
      FOPEN = 0
C                                       Open CP file
      CALL CPINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, ICPRNO,
     *   CPKOLS, CPNUMV, NUMIF, NUMFRQ, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT TABLE'
         GO TO 990
         END IF
      IF (IFQID.GT.0) THEN
         IF ((FREQID.GT.0) .AND. (FREQID.NE.IFQID)) THEN
            CALL TABIO ('CLOS', 0, ICPRNO, BUFFER, BUFFER, IRET)
            IRET = -1
            GO TO 999
         ELSE
            REFMT = FREQID.NE.1
            FREQID = 1
            END IF
         END IF
      FOPEN = 1
C                                       new channel numbers
      IFREQA = MAX (BCHAN, 1)
      IFREQB = MIN (NUMFRQ, ECHAN)
      NEWFRQ = (IFREQB - IFREQA) / CHINC + 1
      ICHAN = IFREQA - BCHAN + 1
C                                       New CP table will cover
C                                       IF range [IFA,IFB].
      IFA = MAX (BIF, 1)
      IFB = MIN (NUMIF, EIF)
      NEWIF = IFB - IFA + 1
C                                       # rows in old table
      NCPROW = BUFFER(5)
C                                       Open up new CP table
      OVER = VER
      CALL CPINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OCPRNO, OKOLS, ONUMV, NEWIF, NEWFRQ, FREQID, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN OUTPUT TABLE'
         GO TO 990
         END IF
      FOPEN = 2
C                                       Loop and copy
      DO 300 I = 1,NCPROW
         CALL TABCP ('READ', BUFFER, ICPRNO, CPKOLS, CPNUMV, NUMIF,
     *      NUMFRQ, SOURCE, SUID, CFLUX, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ OLD TABLE'
            GO TO 990
C                                       Copy this record
         ELSE
            DO 30 J = 1,NEWIF
               JJ = BIF + J - 1
               INX = (J - 1) * NEWFRQ
               LNX = (JJ - 1) * NUMFRQ + IFREQA - CHINC + 1
               IF (INX.NE.LNX) REFMT = .TRUE.
C                                       Renumber IFs/FREQ's
               DO 20 K = 1,NEWFRQ
                  INX = INX + 1
                  LNX = LNX + CHINC
                  CFLUX(1,INX) = CFLUX(1,LNX)
                  CFLUX(2,INX) = CFLUX(2,LNX)
                  CFLUX(3,INX) = CFLUX(3,LNX)
                  CFLUX(4,INX) = CFLUX(4,LNX)
 20               CONTINUE
 30            CONTINUE
C                                       Write output record.
            CALL TABCP ('WRIT', OBUFF, OCPRNO, OKOLS, ONUMV, NEWIF,
     *         NEWFRQ, SOURCE, SUID, CFLUX, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW TABLE'
               GO TO 990
               END IF
            END IF
 300     CONTINUE
C                                       Close both tables
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1300) 'Reformatted CP', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1300) 'Copied CP', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 995
C                                       Error
 990  CALL MSGWRT (6)
C                                       close
 995  IF (FOPEN.GT.0) CALL TABIO ('CLOS', 0, ICPRNO, BUFFER, BUFFER, I)
      IF (FOPEN.GT.1) CALL TABIO ('CLOS', 0, OCPRNO, OBUFF, OBUFF, J)
      IF (IRET.EQ.0) IRET = MAX (I, J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('CPFSEL: ERROR ',I3,1X,A)
 1300 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
      SUBROUTINE PDFSEL (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, CHINC, IFQID, ISUB, JSUB,
     *   BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies and renumbers the IFs and channels in an PD table; can also
C   modify the FQ ID.  Channel increment version
C   Inputs:
C      DISKI    I        Input volume number
C      CNOI     I        Input catalog number
C      DISKO    I        Output volume number
C      CNOO     I        Output catalog number
C      VER      I        Version to check/modify
C      CATIN    I(256)   Input catalog header
C      CATOUT   I(256)   Output catalog header
C      LUNI     I        LUN to use
C      LUNO     I        LUN to use
C      BIF      I        Start IF number
C      EIF      I        End IF number
C      BCHAN    I        First channel
C      ECHAN    I        Last channel
C      IFQID    I        FQ ID to select (output FQID will be 1)
C                           if <= 0 then output FQ ID unchanged.
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER   I(512)   Work buffer
C      OBUFF    I(512)   Work buffer
C   Output:
C      IRET     I        Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER   DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, CHINC, IFQID, ISUB, JSUB,
     *   BUFFER(*), OBUFF(*), IRET
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   MAXPDC
      PARAMETER (MAXPDC = 9)
C
      REAL      DTERMS(2,MAXCIF), PHDIFF(MAXCIF)
      INTEGER   IPDRNO, PDKOLS(MAXPDC), PDNUMV(MAXPDC), OKOLS(MAXPDC),
     *   ONUMV(MAXPDC), NPDROW, OPDRNO, SUBA, FREQID, REFANT(2), NUMANT,
     *   NUMPOL, NUMIF, NUMFRQ, ANT, I, J, K, L, JJ, NEWIF, NEWFRQ, IFA,
     *   IFB, IFREQA, IFREQB, INX, LNX, OVER, FOPEN
      LOGICAL   REFMT
      CHARACTER POLTYP*8
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
      FOPEN = 0
C                                       Open PD file
      CALL PDINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI, IPDRNO,
     *   PDKOLS, PDNUMV, NUMANT, NUMPOL, NUMIF, NUMFRQ, POLTYP, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN INPUT TABLE'
         GO TO 990
         END IF
      FOPEN = 1
      REFMT = .FALSE.
      IFREQA = MAX (BCHAN, 1)
      IFREQB = MIN (NUMFRQ, ECHAN)
      NEWFRQ = (IFREQB - IFREQA) / CHINC + 1
C                                       New PD table will cover
C                                       IF range [IFA,IFB].
      IFA = BIF
      IFB = MIN (NUMIF, EIF)
      NEWIF = IFB - IFA + 1
C                                       # rows in old table
      NPDROW = BUFFER(5)
C                                       Open up new PD table
      OVER = VER
      CALL PDINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *   OPDRNO, OKOLS, ONUMV, NUMANT, NUMPOL, NEWIF, NEWFRQ, POLTYP,
     *   IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1000) IRET, 'OPEN NEW TABLE'
         GO TO 990
         END IF
C                                       Loop and copy
      FOPEN = 2
      DO 300 I = 1,NPDROW
         CALL TABPD ('READ', BUFFER, IPDRNO, PDKOLS, PDNUMV, NUMIF,
     *      NUMFRQ, NUMPOL, ANT, SUBA, ANT, FREQID, REFANT, PHDIFF,
     *      DTERMS, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1000) IRET, 'READ OLD TABLE'
            GO TO 990
            END IF
C                                       Is this FQ ID selected ?
         IF ((IFQID.GT.0) .AND. (FREQID.GT.0) .AND. (IFQID.NE.FREQID))
     *      IRET = -10
         IF ((SUBA.GT.0) .AND. (ISUB.GT.0) .AND. (ISUB.NE.SUBA))
     *      IRET = -10
         IF ((IRET.LT.-3) .OR. (NEWIF.LE.0) .OR. (NEWFRQ.LE.0)) THEN
            REFMT = .TRUE.
C                                       Copy this record
         ELSE
            DO 40 L = 1,NUMPOL
               DO 30 J = 1,NEWIF
                  JJ = BIF + J - 1
                  INX = (J - 1) * NEWFRQ + (L - 1) * NEWFRQ * NEWIF
                  LNX = (JJ - 1) * NUMFRQ + (L - 1) * NUMFRQ * NUMIF +
     *               IFREQA - CHINC + 1
                  IF (INX.NE.LNX) REFMT = .TRUE.
C                                       Renumber IFs/FREQ's
                  DO 20 K = 1,NEWFRQ
                     INX = INX + 1
                     LNX = LNX + CHINC
                     DTERMS(1,INX) = DTERMS(1,LNX)
                     DTERMS(2,INX) = DTERMS(2,LNX)
                     IF (L.EQ.1) PHDIFF(INX) = PHDIFF(LNX)
 20                  CONTINUE
 30               CONTINUE
 40            CONTINUE
C                                       Set output FQ ID
            IF (IFQID.GT.0) FREQID = 1
            IF (JSUB.GE.0) SUBA = JSUB
C                                       Write output record.
            CALL TABPD ('WRIT', OBUFF, OPDRNO, OKOLS, ONUMV, NEWIF,
     *         NEWFRQ, NUMPOL, ANT, SUBA, FREQID, REFANT, PHDIFF,
     *         DTERMS, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'WRITE NEW TABLE'
               GO TO 990
               END IF
            END IF
 300     CONTINUE
C                                       Close both tables
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1300) 'Reformatted PD', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1300) 'Copied PD', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 995
C                                       Error
 990  CALL MSGWRT (6)
C                                       close
 995  IF (FOPEN.GT.0) CALL TABIO ('CLOS', 0, IPDRNO, BUFFER, BUFFER, I)
      IF (FOPEN.GT.1) CALL TABIO ('CLOS', 0, OPDRNO, OBUFF, OBUFF, J)
      IF (IRET.EQ.0) IRET = MAX (I, J)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('PDFSEL: ERROR ',I3,1X,A)
 1300 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
      SUBROUTINE DIDDLE (NUMVIS, VIS, INCX, RESULT, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
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
      INTEGER   NUMVIS, INCX, IRET
      REAL      VIS(INCX,*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, I, LF
      INCLUDE 'UVDEC.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         DO 40 JIF = 1,NIF
            DO 30 JF = 1,NF
               LF = (JF - 1) * CHINC + BCHAN
               DO 20 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (LF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
                  DO 10 I = 1,INCX
                     RESULT(I,INDEXO) = VIS(I,INDEXI)
 10                  CONTINUE
 20               CONTINUE
 30            CONTINUE
 40         CONTINUE
C                                       last call - no vis
      ELSE
         NUMHIS = 3
         WRITE (HISCRD(1),1100) BCHAN
         WRITE (HISCRD(2),1101) ECHAN
         WRITE (HISCRD(3),1102) CHINC
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('BCHAN =',I5,5X,'/ Begin channel number')
 1101 FORMAT ('ECHAN =',I5,5X,'/ End channel number')
 1102 FORMAT ('CHINC =',I5,5X,'/ Channel number increment')
      END
      SUBROUTINE FGSELD (DISKI, CNOI, DISKO, CNOO, VER, CATIN, CATOUT,
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, CHINC, TB, TE, IFQID, ISUB,
     *   JSUB, BUFFER, OBUFF, IRET)
C-----------------------------------------------------------------------
C   Copies and renumbers the IFs in an FG table, can also modify the
C   FQ ID.   Local version includes CHINC
C   Inputs:
C      DISKI           I       Input volume number
C      CNOI            I       Input catalog number
C      DISKO           I       Output volume number
C      CNOO            I       Output catalog number
C      VER             I       Version to check/modify
C      CATIN(256)      I       Input catalog header
C      CATOUT(256)     I       Output catalog header
C      LUNI            I       LUN to use
C      LUNO            I       LUN to use
C      BIF             I       Start IF number
C      EIF             I       End IF number
C      BCHAN           I       First channel
C      ECHAN           I       Last channel
C      TB       D        Beginning time in days
C      TE       D        Ending time in days
C      IFQID           I       FQ ID to select (set to 1 on output)
C                              if <= 0 then output value unchanged.
C      ISUB     I        Selected subarray (= 0 any)
C      JSUB     I        Output subarray number (< 0 => NO CHANGE)
C   Input/Output:
C      BUFFER          I(*)    Work buffer
C      OBUFF           I(*)    Work buffer
C   Output:
C      IRET            I       Error, 0 => OK
C-----------------------------------------------------------------------
      INTEGER DISKI, CNOI, DISKO, CNOO, VER, CATIN(256), CATOUT(256),
     *   LUNI, LUNO, BIF, EIF, BCHAN, ECHAN, CHINC, IFQID, ISUB, JSUB,
     *   BUFFER(*), OBUFF(*), IRET
      DOUBLE PRECISION TB, TE
C
      INCLUDE 'INCS:PUVD.INC'
      CHARACTER REASON*24
      INTEGER   OVER, IFGRNO, FGKOLS(MAXFGC), FGNUMV(MAXFGC),
     *   OKOLS(MAXFGC), ONUMV(MAXFGC), NFGROW, I, OFGRNO,
     *   SOURID, SUBA, FREQID, ANTS(2), IFS(2), CHANS(2)
      LOGICAL PFLAGS(4), DROP, REFMT
      REAL    TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
C-----------------------------------------------------------------------
C                                       Open FG file
      CALL FLGINI ('READ', BUFFER, DISKI, CNOI, VER, CATIN, LUNI,
     *  IFGRNO, FGKOLS, FGNUMV, IRET)
      IF (IRET.EQ.2) THEN
         IRET = 0
         GO TO 999
         END IF
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       # rows in old table
      NFGROW = BUFFER(5)
      REFMT = JSUB.GE.0
C                                       Open up new FG table
      OVER = VER
      CALL FLGINI ('WRIT', OBUFF, DISKO, CNOO, OVER, CATOUT, LUNO,
     *  OFGRNO, OKOLS, ONUMV, IRET)
      IF (IRET.NE.0) THEN
         WRITE (MSGTXT,1010) IRET
         GO TO 990
         END IF
C                                       Loop and copy
      DO 100 I = 1, NFGROW
         CALL TABFLG ('READ', BUFFER, IFGRNO, FGKOLS, FGNUMV,
     *      SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *      REASON, IRET)
         IF (IRET.GT.0) THEN
            WRITE (MSGTXT,1020) IRET
            GO TO 990
            END IF
         DROP = IRET.LT.0
C                                       Is this FQ ID selected ?
         DROP = DROP .OR. ((IFQID.GT.0) .AND. (IFQID.NE.FREQID) .AND.
     *      (FREQID.GT.0))
C                                       Is this FQ ID selected ?
         DROP = DROP .OR. ((ISUB.GT.0) .AND. (ISUB.NE.SUBA) .AND.
     *      (SUBA.GT.0))
C                                       timerange
         DROP = DROP .OR. (TB.GT.TIMER(2)) .OR. (TE.LT.TIMER(1))
C                                       Renumber IFs
         IF ((BIF.GT.0) .AND. (EIF.GT.0)) THEN
            IF (IFS(1).GT.0) THEN
               IF (IFS(1).GT.EIF) THEN
                  DROP = .TRUE.
               ELSE IF (BIF.GT.1) THEN
                  REFMT = .TRUE.
                  IFS(1) = MAX (1, IFS(1)-BIF+1)
                  END IF
               END IF
            IF (IFS(2).GT.0) THEN
               IF (BIF.GT.1) THEN
                  REFMT = .TRUE.
                  IFS(2) = IFS(2) - BIF + 1
                  DROP = DROP .OR. (IFS(2).LE.0)
                  END IF
               IF (IFS(2).GT.EIF-BIF+1) THEN
                  REFMT = .TRUE.
                  IFS(2) = EIF - BIF + 1
                  END IF
               END IF
            END IF
C                                       Renumber channels
         IF ((BCHAN.GT.0) .AND. (ECHAN.GT.0)) THEN
            IF (CHANS(1).GT.0) THEN
               IF (CHANS(1).GT.ECHAN) THEN
                  DROP = .TRUE.
C                                       round up in lower limit
               ELSE IF ((BCHAN.GT.1) .OR. (CHINC.GT.1)) THEN
                  REFMT = .TRUE.
                  CHANS(1) = (CHANS(1) + CHINC - 1 - BCHAN) / CHINC + 1
                  CHANS(1) = MAX (1, CHANS(1))
                  END IF
               END IF
            IF (CHANS(2).GT.0) THEN
C                                       round down in upper limit
               IF ((BCHAN.GT.1) .OR. (CHINC.GT.1)) THEN
                  REFMT = .TRUE.
                  CHANS(2) = (CHANS(2) - BCHAN) / CHINC + 1
                  DROP = DROP .OR. (CHANS(2).LE.0)
                  END IF
               IF (CHANS(2).GT.(ECHAN-BCHAN)/CHINC+1) THEN
                  REFMT = .TRUE.
                  CHANS(2) = (ECHAN - BCHAN) / CHINC + 1
                  END IF
               END IF
            END IF
C                                       May ignore some records
         IF (DROP) THEN
            REFMT = .TRUE.
C                                       Write new one
         ELSE
            IF (IFQID.GT.0) FREQID = 1
            IF (JSUB.GE.0) SUBA = JSUB
            CALL TABFLG ('WRIT', OBUFF, OFGRNO, OKOLS, ONUMV,
     *         SOURID, SUBA, FREQID, ANTS, TIMER, IFS, CHANS, PFLAGS,
     *         REASON, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1020) IRET
               GO TO 990
               END IF
            END IF
 100     CONTINUE
C                                       Close both tables
      CALL TABIO ('CLOS', 0, IFGRNO, BUFFER, BUFFER, IRET)
      CALL TABIO ('CLOS', 0, OFGRNO, OBUFF, OBUFF, IRET)
      IF ((MSGSUP.LT.31990) .OR. (MSGSUP.GE.32000)) THEN
         IF (REFMT) THEN
            WRITE (MSGTXT,1100) 'Reformatted FG', DISKI, CNOI, VER,
     *         DISKO, CNOO, OVER
         ELSE
            WRITE (MSGTXT,1100) 'Copied FG', DISKI, CNOI, VER, DISKO,
     *         CNOO, OVER
            END IF
         CALL MSGWRT (3)
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (6)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('FGSEL: ERROR ',I3,' RETURNED FROM FLGINI')
 1020 FORMAT ('FGSEL: ERROR ',I3,' RETURNED FROM TABFLG')
 1100 FORMAT (A,' file from vol/cno/vers',I3,I5,I4,' to',I3,I5,I4)
      END
