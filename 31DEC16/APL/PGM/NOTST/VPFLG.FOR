LOCAL INCLUDE 'VPFLG.INC'
C                                       Local include for VPFLG
      INCLUDE 'INCS:ZPBUFSZ.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2), XOPCOD
      REAL      XSIN, XDISIN, XSOUT, XDISO, XBCHAN, XECHAN, DOIFS,
     *   XCENT, BUFF1(UVBFSS), BUFF2(UVBFSS), DIFPIX
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, NUMHIS, JBUFSZ, ILOCWT,
     *   CATOLD(256), INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO, LRECI,
     *   LRECO, NRPRMI, NRPRMO, BCHAN, ECHAN, IBUFF1(UVBFSS),
     *   IBUFF2(UVBFSS)
      LOGICAL   ISCOMP
      DOUBLE PRECISION UVSCAL
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, HISCRD(10)*64,
     *   OPCODE*4
      EQUIVALENCE (BUFF1, IBUFF1), (BUFF2, IBUFF2)
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, XBCHAN, XECHAN, DOIFS, XOPCOD, XCENT
      COMMON /VPFLGP/ CATOLD, UVSCAL, SEQIN, SEQOUT, DISKIN, DISKO,
     *   NUMHIS, ILOCWT, INCSI, INCFI, INCIFI, INCSO, INCFO, INCIFO,
     * LRECI, LRECO, NRPRMI, NRPRMO, ISCOMP, BCHAN, ECHAN, DIFPIX
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, HISCRD, OPCODE
      COMMON /BUFRS/ BUFF1, BUFF2, JBUFSZ
C                                       End local include for VPFLG
LOCAL END
      PROGRAM VPFLG
C-----------------------------------------------------------------------
C! Flags all correlators when 1 is flagged
C# UV EDITING
C-----------------------------------------------------------------------
C;  Copyright (C) 2001, 2003, 2005, 2008, 2011, 2014-2015
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
C   VPFLG flagss all correlators of an IF sample when one is flagged.
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
C      BCHAN          BCHAN         Begin channel to check
C      ECHAN          ECHAN         End channel to check
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  IRET
      INCLUDE 'VPFLG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA PRGM /'VPFLG '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL VPFLIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Call routine that sends data
C                                       to the user routine.
      CALL SENDUV (IRET)
      IF (IRET.NE.0) GO TO 990
      CALL VPFHIS
C                                       Close down files, etc.
 990  CALL DIE (IRET, IBUFF1)
C
 999  STOP
      END
      SUBROUTINE VPFLIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   VPFLIN gets input parameters for VPFLG and creates an output file
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
C   See prologue comments in VPFLG for more details.
C
C   To change the adverb list sent to this task change:
C   1)  the inputs file.
C   2)  the contents of COMMON /INPARM/.  Remember all adverbs are sent
C       as R, INNAME etc. are 12 char. 3 words;
C       INCLASS etc. are 6 char., 2 words.
C       Values will be filled into COMMON /INPARM/ in the order
C       specified in the inputs file.
C   3)  If the first adverb is not INNAME (NAMEIN) then replace
C       NAMEIN in the call to GTPARM with the name of the first
C       adverb.
C   4)  Change the value of NPARM sent to GTPARM to the number of
C       R words desired.
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER  STAT*4, BLANK*6, PTYPE*2
      INTEGER   OLDCNO, IROUND, NPARM, IERR, INCX
      LOGICAL   T
      INCLUDE 'VPFLG.INC'
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
      NPARM = 19
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
         END IF
C                                       Restart AIPS
      IF (RQUICK) CALL RELPOP (JERR, IBUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
      CALL H2CHR (12, 1, XNAMOU, NAMOUT)
      CALL H2CHR (6, 1, XCLAOU, CLAOUT)
      CALL H2CHR (4, 1, XOPCOD, OPCODE)
      SEQIN = IROUND (XSIN)
      SEQOUT = IROUND (XSOUT)
      DISKIN = IROUND (XDISIN)
      DISKO = IROUND (XDISO)
C                                       Create new file.
C                                       Get CATBLK from old file.
      OLDCNO = 1
      PTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   PTYPE, NLUSER, STAT, IBUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', IBUFF1, IERR)
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
      IF ((CATBLK(KINAX+JLOCS).NE.4) .AND. (OPCODE.EQ.'CROS')) THEN
         MSGTXT = 'CROS REQUESTED WITHOUT FULL POLARIZATIONS, QUITTING'
         JERR = 9
         GO TO 990
         END IF
C                                       channel check range
      BCHAN = XBCHAN + 0.1
      BCHAN = MAX (1, MIN (BCHAN, CATBLK(KINAX+JLOCF)))
      ECHAN = XECHAN + 0.1
      IF (ECHAN.LT.BCHAN) ECHAN = CATBLK(KINAX+JLOCF)
C                                       Save input file info
      INCX = CATBLK(KINAX)
      LRECI = LREC
      NRPRMI = NRPARM
      INCSI = INCS / INCX
      INCFI = INCF / INCX
      INCIFI = INCIF / INCX
C                                       center frequencies
      IF (JLOCF.LT.0) XCENT = -1.
      IF (XCENT.GT.0.0) THEN
         INCX = CATBLK(KINAX+JLOCF) / 2 + 1
         DIFPIX = (INCX-CATR(KRCRP+JLOCF))
         CATD(KDCRV+JLOCF) = CATD(KDCRV+JLOCF) + CATR(KRCIC+JLOCF) *
     *      DIFPIX
         CATR(KRCRP+JLOCF) = INCX
         UVSCAL = CATD(KDCRV+JLOCF) / FREQ
      ELSE
         UVSCAL = 1.0D0
         DIFPIX = 0.0
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
      CALL UVCREA (DISKO, CCNO, IBUFF1, IERR)
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
         CALL CATIO ('READ', DISKO, CCNO, CATBLK, 'WRIT', IBUFF1, IERR)
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
     *   PTYPE, NLUSER, 'READ', IBUFF1, IERR)
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
 1000 FORMAT ('VPFLIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('VPFLIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE SENDUV (IRET)
C-----------------------------------------------------------------------
C   SENDUV sends uv data one point at a time to the user supplied
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
     *   KBIND, NIOUT, NIOLIM, IBIND, I, INCX, BO, VO, NUMVIS, XCOUNT,
     *   NCORI, NCORO, NCOPY, RNXRET
      LOGICAL   T, F
      INCLUDE 'VPFLG.INC'
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
C                                       make an index table
      CALL RNXGET (DISKIN, FCNO(NCFILE), CATOLD)
      CALL RNXINI (DISKO, CCNO, CATBLK, RNXRET)
C                                       Loop
 100  CONTINUE
C                                       Read vis. record.
         CALL UVDISK ('READ', LUNI, INDI, BUFF1, INIO, IBIND, IRET)
         IF (IRET.NE.0) THEN
            WRITE (MSGTXT,1100) IRET
            GO TO 990
            END IF
         IPTRI = IBIND
C                                       Loop over buffer
         DO 190 I = 1,INIO
            NUMVIS = NUMVIS + 1
            BUFF1(IPTRI+ILOCU) = BUFF1(IPTRI+ILOCU) * UVSCAL
            BUFF1(IPTRI+ILOCV) = BUFF1(IPTRI+ILOCV) * UVSCAL
            BUFF1(IPTRI+ILOCW) = BUFF1(IPTRI+ILOCW) * UVSCAL
C                                       Compressed data.
            IF (ISCOMP) THEN
               CALL ZUVXPN (NCORI, BUFF1(IPTRI+NRPRMI),
     *            BUFF1(IPTRI+ILOCWT), CBUFF)
               IF (OPCODE.EQ.'CROS') THEN
                  CALL DIDCRS (NUMVIS, CBUFF, INCX, RESULT, IRET)
               ELSE
                  CALL DIDDLE (NUMVIS, CBUFF, INCX, RESULT, IRET)
                  END IF
C                                       Un compressed data
            ELSE
               IF (OPCODE.EQ.'CROS') THEN
                  CALL DIDCRS (NUMVIS, BUFF1(IPTRI+NRPRMI), INCX,
     *               RESULT, IRET)
               ELSE
                  CALL DIDDLE (NUMVIS, BUFF1(IPTRI+NRPRMI), INCX,
     *               RESULT, IRET)
                  END IF
               END IF
C                                       Error (fatal)
            IF (IRET.GT.0) THEN
               WRITE (MSGTXT,1120) IRET
               GO TO 990
C                                       Copy to output.
            ELSE IF (IRET.EQ.0) THEN
               XCOUNT = XCOUNT + 1.0D0
               CALL RCOPY (NRPRMO, BUFF1(IPTRI), BUFF2(IPTRO))
C                                       update NX table
               CALL RNXUPD (BUFF2(IPTRO), RNXRET)
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
         IF (INIO.GT.0) GO TO 100
C                                       Final call to DIDDLE.
         NUMVIS = -1
         IF (OPCODE.EQ.'CROS') THEN
            CALL DIDCRS (NUMVIS, BUFF1, INCX, RESULT, IRET)
         ELSE
            CALL DIDDLE (NUMVIS, BUFF1, INCX, RESULT, IRET)
            END IF
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
 1000 FORMAT ('SENDUV: ERROR',I3,' OPEN-FOR-READ VIS FILE')
 1010 FORMAT ('SENDUV: ERROR',I3,' OPEN-FOR-WRITE VIS FILE')
 1020 FORMAT ('SENDUV: ERROR',I3,' INIT-FOR-WRITE VIS FILE')
 1030 FORMAT ('SENDUV: ERROR',I3,' INIT-FOR-READ VIS FILE')
 1100 FORMAT ('SENDUV: ERROR',I3,' READING VIS FILE')
 1120 FORMAT ('SENDUV: DIDDLE ERROR',I3)
 1150 FORMAT ('SENDUV: ERROR',I3,' WRITING VIS FILE')
      END
      SUBROUTINE VPFHIS
C-----------------------------------------------------------------------
C   VPFHIS copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72, LABEL*8
      INTEGER   LUN1, LUN2, IERR, I, NONOT
      LOGICAL   T
      INCLUDE 'VPFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27,28/
      DATA T /.TRUE./
      DATA NONOT, NOTTYP /1, 'FG'/
C-----------------------------------------------------------------------
C                                        Copy tables
      CALL FGTAB (LUN1, LUN2, DISKIN, DISKO, FCNO(2), FCNO(1), DOIFS,
     *   OPCODE, CATBLK, IBUFF1, IBUFF2, NUMHIS, HISCRD, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200) 'FQ'
         CALL MSGWRT (6)
         END IF
C                                       Write History.
      CALL HIINIT (3)
C                                       Copy/open history file.
      CALL HISCOP (LUN1, LUN2, DISKIN, DISKO, FCNO(NCFILE),
     *   FCNO(NCFILE-1), CATBLK, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1000) IERR
         CALL MSGWRT (6)
         GO TO 200
         END IF
C                                       New history
      CALL HENCO1 (TSKNAM, NAMEIN, CLAIN, SEQIN, DISKIN, LUN2, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
      CALL HENCOO (TSKNAM, NAMOUT, CLAOUT, SEQOUT, DISKO, LUN2, IBUFF2,
     *   IERR)
      IF (IERR.NE.0) GO TO 200
C                                      Add any other history.
      IF (NUMHIS.LE.0) GO TO 200
         WRITE (LABEL,1010) TSKNAM
         DO 50 I = 1,NUMHIS
            HILINE = LABEL // HISCRD(I)
            CALL HIADD (LUN2, HILINE, IBUFF2, IERR)
            IF (IERR.NE.0) GO TO 200
 50         CONTINUE
C                                       Close HI file
 200  CALL HICLOS (LUN2, T, IBUFF2, IERR)
C                                        Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200) 'REST OF'
         CALL MSGWRT (6)
         END IF
C                                       correct for FQCENTER
      CALL CENTFQ (DISKO, FCNO(1), DIFPIX, IBUFF1, IBUFF2, IERR)
      IF (IERR.GT.0) THEN
         MSGTXT = 'VPFHIS: ERROR CORRECTING FQ TABLE'
         CALL MSGWRT (6)
         END IF
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   IBUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('VPFHIS: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,'/ ')
 1200 FORMAT ('VPFHIS: ERROR COPYING ',A,' TABLES')
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
C      BCHAN      I       Start channel to test
C      ECHAN      I       End channel to test
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
      INTEGER   NUMVIS, INCX, IRET
      REAL      VIS(INCX,*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, I
      DOUBLE PRECISION FLAGED
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   FLAG(MAXIF), FLAGS
      INCLUDE 'VPFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE FLAGED
      DATA FLAGED /0.0D0/
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         DO 25 JIF = 1,NIF
            FLAG(JIF) = .FALSE.
C                                       Do we need to flag?
            IF (INCX.GE.3) THEN
               DO 20 JF = BCHAN,ECHAN
                  FLAGS = .FALSE.
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI + 1
                  DO 10 JS = 1,NS
                     IF (VIS(3,INDEXI).LE.0.0) THEN
                        FLAGS = .TRUE.
                        IF (DOIFS.GT.0.0) THEN
                           FLAG(JIF) = .TRUE.
                           GO TO 25
                           END IF
                        END IF
                     INDEXI = INDEXI + INCSI
 10                  CONTINUE
                  IF (FLAGS) THEN
                     INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI + 1
                     DO 15 JS = 1,NS
                        IF (VIS(3,INDEXI).GT.0.0) THEN
                           FLAGED = FLAGED + 1.0D0
                           VIS(3,INDEXI) = -ABS (VIS(3,INDEXI))
                           END IF
                        INDEXI = INDEXI + INCSI
 15                     CONTINUE
                     END IF
 20               CONTINUE
               END IF
 25         CONTINUE
C                                       flag all IF  if 1
         IF (DOIFS.GT.1.001) THEN
            FLAGS = .FALSE.
            DO 30 JIF = 1,NIF
               IF (FLAG(JIF)) FLAGS = .TRUE.
 30            CONTINUE
            DO 35 JIF = 1,NIF
               FLAG(JIF) = FLAGS
 35            CONTINUE
            END IF
C                                       Copy data
         DO 70 JIF = 1,NIF
            DO 60 JF = 1,NF
               DO 50 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
                  DO 40 I = 1,INCX
                     RESULT(I,INDEXO) = VIS(I,INDEXI)
C                                       flag it
                     IF ((I.EQ.3) .AND. (FLAG(JIF))) THEN
                        IF (RESULT(I,INDEXO).GT.0.0) FLAGED =
     *                     FLAGED + 1.D0
                        RESULT(I,INDEXO) = - ABS (RESULT(I,INDEXO))
                        END IF
 40                  CONTINUE
 50               CONTINUE
 60            CONTINUE
 70         CONTINUE
C                                       last call - no vis
      ELSE
         NUMHIS = 2
         WRITE (HISCRD(1),1100) FLAGED
         CALL REFRMT (HISCRD(1), '_', I)
         MSGTXT = HISCRD(1)
         CALL MSGWRT (4)
         WRITE (HISCRD(2),1101) DOIFS
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Flagged',F15.0,' additional correlators')
 1101 FORMAT ('DOIFS =',F3.0,5X,'/ Flag all IFs if 1 bad ?')
      END
      SUBROUTINE DIDCRS (NUMVIS, VIS, INCX, RESULT, IRET)
C-----------------------------------------------------------------------
C   Inputs:
C      NUMVIS  I    Visibility number, -1 => final call, no data
C                   passed but allows any operations to be completed.
C      VIS     R(INCX,*)  Visibilities in order real, imaginary, weight
C                   (Jy, Jy, unitless).  Weight <= 0 => flagged.
C                   NOTE: INCX may be any value .GE. 2
C   Inputs from COMMON:
C      BCHAN      I       Start channel to test
C      ECHAN      I       End channel to test
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
      INTEGER   NUMVIS, INCX, IRET
      REAL      VIS(INCX,*), RESULT(INCX,*)
C
      INTEGER   JIF, JF, JS, NIF, NF, NS, INDEXO, INDEXI, I
      DOUBLE PRECISION FLAGED
      INCLUDE 'INCS:PUVD.INC'
      LOGICAL   FLAG(MAXIF), FLAGS
      INCLUDE 'VPFLG.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      SAVE FLAGED
      DATA FLAGED /0.0D0/
C-----------------------------------------------------------------------
      IRET = 0
      IF (NUMVIS.GT.0) THEN
         NS = 1
         NIF = 1
         NF = 1
         IF (JLOCS.GE.0) NS = CATBLK(KINAX+JLOCS)
         IF (JLOCIF.GE.0) NIF = CATBLK(KINAX+JLOCIF)
         IF (JLOCF.GE.0) NF = CATBLK(KINAX+JLOCF)
         DO 25 JIF = 1,NIF
            FLAG(JIF) = .FALSE.
C                                       Do we need to flag?
            IF (INCX.GE.3) THEN
               DO 20 JF = BCHAN,ECHAN
                  FLAGS = .FALSE.
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI + 1
                  DO 10 JS = 1,2
                     IF (VIS(3,INDEXI).LE.0.0) THEN
                        FLAGS = .TRUE.
                        IF (DOIFS.GT.0.0) THEN
                           FLAG(JIF) = .TRUE.
                           GO TO 25
                           END IF
                        END IF
                     INDEXI = INDEXI + INCSI
 10                  CONTINUE
                  IF (FLAGS) THEN
                     INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI + 1 +
     *                  2 * INCSI
                     DO 15 JS = 3,4
                        IF (VIS(3,INDEXI).GT.0.0) THEN
                           FLAGED = FLAGED + 1.0D0
                           VIS(3,INDEXI) = -ABS (VIS(3,INDEXI))
                           END IF
                        INDEXI = INDEXI + INCSI
 15                     CONTINUE
                     END IF
 20               CONTINUE
               END IF
 25         CONTINUE
C                                       flag all IF  if 1
         IF (DOIFS.GT.1.001) THEN
            FLAGS = .FALSE.
            DO 30 JIF = 1,NIF
               IF (FLAG(JIF)) FLAGS = .TRUE.
 30            CONTINUE
            DO 35 JIF = 1,NIF
               FLAG(JIF) = FLAGS
 35            CONTINUE
            END IF
C                                       Copy data
         DO 70 JIF = 1,NIF
            DO 60 JF = 1,NF
               DO 50 JS = 1,NS
                  INDEXI = (JIF-1) * INCIFI + (JF-1) * INCFI +
     *               (JS-1) * INCSI + 1
                  INDEXO = (JIF-1) * INCIFO + (JF-1) * INCFO +
     *               (JS-1) * INCSO + 1
                  DO 40 I = 1,INCX
                     RESULT(I,INDEXO) = VIS(I,INDEXI)
C                                       flag it
                     IF ((I.EQ.3) .AND. (FLAG(JIF)) .AND. (JS.GT.2))
     *                  THEN
                        IF (RESULT(I,INDEXO).GT.0.0) FLAGED =
     *                     FLAGED + 1.D0
                        RESULT(I,INDEXO) = - ABS (RESULT(I,INDEXO))
                        END IF
 40                  CONTINUE
 50               CONTINUE
 60            CONTINUE
 70         CONTINUE
C                                       last call - no vis
      ELSE
         NUMHIS = 2
         WRITE (HISCRD(1),1100) FLAGED
         CALL REFRMT (HISCRD(1), '_', I)
         MSGTXT = HISCRD(1)
         CALL MSGWRT (4)
         WRITE (HISCRD(2),1101) DOIFS
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('Flagged',F15.0,' additional correlators')
 1101 FORMAT ('DOIFS =',F3.0,5X,'/ Flag all IFs if 1 bad ?')
      END
      SUBROUTINE FGTAB (LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD, CNONEW,
     *   DOIFS, OPCODE, CATNEW, BUFF1, BUFF2, NUMHIS, HISCRD, IRET)
C-----------------------------------------------------------------------
C   FGTAB copies all FQ extension file(s) revising the flagging to be
C   all polarization.
C   Inputs:
C      LUNOLD  I       LUN for old file
C      LUNNEW  I       LUN for new file
C      VOLOLD  I       Disk number for old file.
C      VOLNEW  I       Disk number for new file.
C      CNOOLD  I       Catalog slot number for old file
C      CNONEW  I       Catalog slot number for new file
C      DOIFS   R       Flag all IFs if 1 flagged
C   In/out:
C      CATNEW  I(256)  Catalog header for new file.
C   Output:
C      BUFF1   I(1024) Work buffer
C      BUFF2   I(1024) Work buffer
C      IRET    I       Return error code  0 => ok, otherwise TABCOP
C                                             or 10*CATIO error.
C-----------------------------------------------------------------------
      INTEGER   LUNOLD, LUNNEW, VOLOLD, VOLNEW, CNOOLD, CNONEW,
     *   BUFF1(*), BUFF2(*), CATNEW(256), NUMHIS, IRET
      CHARACTER OPCODE*4, HISCRD(*)*(*)
      REAL      DOIFS
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IVERI, IVERO, IER, IEXT, JEXT, OLDNUM, NFIX, CATB(256),
     *   IFGRNO, FGKOLS(MAXFGC), FGNUMV(MAXFGC), OKOLS(MAXFGC),
     *   ONUMV(MAXFGC), NFGROW, I, SOURID, ANTNO(2), SUBA, FREQID,
     *   IFS(2), CHANS(2), OFGRNO
      LOGICAL   TABLE, EXIST, FITASC, PFLAGS(4), TFLAGS(4)
      CHARACTER OLDTYP*2, REASON*24
      REAL      TIMER(2)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA TFLAGS /4*.TRUE./
C-----------------------------------------------------------------------
C                                       Get old CATBLK in BUFF2.
      CALL CATIO ('READ', VOLOLD, CNOOLD, CATB, 'REST', BUFF2, IRET)
      IF ((IRET.GE.1) .AND. (IRET.LE.4)) THEN
         WRITE (MSGTXT,1000) IRET, 'READ', 'OLD CATBLK'
         CALL MSGWRT (6)
         IRET = 10 * IRET
         GO TO 999
         END IF
C                                       header ext format
      CALL FXHDEX (BUFF2)
C                                       Get FQ extension file info
      DO 10 IEXT = 1,KIEXTN
         OLDTYP = ' '
         CALL H2CHR (2, 1, CATB(KHEXT+IEXT-1), OLDTYP)
         OLDNUM = CATB(KIVER+IEXT-1)
         IF ((OLDNUM.GT.0) .AND. (OLDTYP.EQ.'FG')) GO TO 20
 10      CONTINUE
      IRET = 0
      GO TO 999
C                                       Loop, copying tables.
C                                       Copy each table independently
 20   DO 50 JEXT = 1,OLDNUM
C                                       See if files exist and are
C                                       wanted.
         CALL ISTAB (OLDTYP, VOLOLD, CNOOLD, JEXT, LUNOLD, BUFF1, TABLE,
     *      EXIST, FITASC, IER)
         IF (TABLE .AND. EXIST .AND. (IER.EQ.0)) THEN
            IVERI = JEXT
            IVERO = JEXT
            NFIX = 0
C                                       Copy
            CALL FLGINI ('READ', BUFF1, VOLOLD, CNOOLD, IVERI, CATB,
     *         LUNOLD, IFGRNO, FGKOLS, FGNUMV, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT', 'OLD FG'
               GO TO 990
               END IF
C                                       # rows in old table
            NFGROW = BUFF1(5)
C                                       Open up new FG table
            CALL FLGINI ('WRIT', BUFF2, VOLNEW, CNONEW, IVERO, CATNEW,
     *         LUNNEW, OFGRNO, OKOLS, ONUMV, IRET)
            IF (IRET.NE.0) THEN
               WRITE (MSGTXT,1000) IRET, 'INIT', 'NEW FG'
               GO TO 990
               END IF
C                                       Loop and copy
            DO 30 I = 1,NFGROW
               CALL TABFLG ('READ', BUFF1, IFGRNO, FGKOLS, FGNUMV,
     *            SOURID, SUBA, FREQID, ANTNO, TIMER, IFS, CHANS,
     *            PFLAGS, REASON, IRET)
               IF (IRET.GT.0) THEN
                  WRITE (MSGTXT,1000) IRET, 'READ', 'OLD FG'
                  GO TO 990
                  END IF
               IF (OPCODE.EQ.'CROS') THEN
                  CALL LCOPY (4, PFLAGS, TFLAGS)
                  IF ((PFLAGS(1)) .OR. (PFLAGS(2))) THEN
                     IF ((.NOT.PFLAGS(3)) .OR. (.NOT.PFLAGS(4)))
     *               NFIX = NFIX + 1
                     TFLAGS(3) = .TRUE.
                     TFLAGS(4) = .TRUE.
                     IF (DOIFS.GT.0.0) THEN
                        IFS(1) = 0
                        IFS(2) = 0
                        END IF
                     END IF
               ELSE
                  IF (DOIFS.GT.0.0) THEN
                     IFS(1) = 0
                     IFS(2) = 0
                     END IF
                  IF ((.NOT.PFLAGS(1)) .OR. (.NOT.PFLAGS(2)) .OR.
     *               (.NOT.PFLAGS(3)) .OR. (.NOT.PFLAGS(4)))
     *               NFIX = NFIX + 1
                  END IF
               IF (IRET.EQ.0) THEN
                  CALL TABFLG ('WRIT', BUFF2, OFGRNO, OKOLS, ONUMV,
     *               SOURID, SUBA, FREQID, ANTNO, TIMER, IFS, CHANS,
     *               TFLAGS, REASON, IRET)
                  IF (IRET.NE.0) THEN
                     WRITE (MSGTXT,1000) IRET, 'WRIT', 'NEW FG'
                     GO TO 990
                     END IF
                  END IF
 30            CONTINUE
            IF (NUMHIS.LT.10) THEN
               NUMHIS = NUMHIS + 1
               WRITE (HISCRD(NUMHIS),1030) IVERI, IVERO, NFIX, NFGROW
               CALL REFRMT (HISCRD(NUMHIS), '_', I)
               MSGTXT = HISCRD(NUMHIS)
               CALL MSGWRT (3)
               END IF
C                                       Close both tables
            CALL TABIO ('CLOS', 0, IFGRNO, BUFF1, BUFF1, IRET)
            CALL TABIO ('CLOS', 0, OFGRNO, BUFF2, BUFF2, IRET)
            END IF
 50      CONTINUE
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('FGTAB: ERROR',I5,1X,A,'ING ',A)
 1030 FORMAT ('Copied FG ver',I4,' to',I4,' fixing',I8,' of',I8,' rows')
      END
