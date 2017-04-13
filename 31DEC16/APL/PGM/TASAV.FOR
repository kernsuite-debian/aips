LOCAL INCLUDE 'TASAV.INC'
      HOLLERITH XNAMEI(3), XCLAIN(2), XNAMOU(3), XCLAOU(2)
      REAL      XSIN, XDISIN, XSOUT, XDISO, BUFF1(4096), BUFF2(4096)
      INTEGER   SEQIN, SEQOUT, DISKIN, DISKO, JBUFSZ, CATOLD(256)
      CHARACTER NAMEIN*12, CLAIN*6, NAMOUT*12, CLAOUT*6, DATYPE*2
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, XNAMOU, XCLAOU,
     *   XSOUT, XDISO, SEQIN, SEQOUT, DISKIN, DISKO
      COMMON /CHARPM/ NAMEIN, CLAIN, NAMOUT, CLAOUT, DATYPE
      COMMON /BUFRS/ CATOLD, BUFF1, BUFF2, JBUFSZ
LOCAL END
      PROGRAM TASAV
C-----------------------------------------------------------------------
C! Task to copy all tables to a dummy UV file.
C# Utility UV UV-util VLA VLB
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003
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
C   TASAV copies all extension files to a dummy file. It is useful if a
C   user wishes to save only the tables on a FITS tape for instance.
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
      INCLUDE 'TASAV.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DCAT.INC'
C
      DATA PRGM /'TASAV '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       create output file if nec.
      CALL TBLSIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Copy tables & write HI
C                                       entry
      CALL TBSVHI
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE TBLSIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   TBLSIN gets input parameters for TASAV and creates an output file
C   if necessary.
C   Inputs:
C      PRGN    C*6  Program name
C   Output:
C      JERR    I    Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C-----------------------------------------------------------------------
      INTEGER   JERR
      CHARACTER PRGN*6
C
      CHARACTER STAT*4, BLANK*6
      INTEGER   OLDCNO, IROUND, NPARM, IERR, I, J
      REAL      BLC(7), TRC(7), XINC, YINC
      INCLUDE 'TASAV.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      DATA BLANK  /' '/
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = PRGN
      CALL ZDCHIN (.TRUE.)
      CALL VHDRIN
      JBUFSZ = 4096 * 2
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 14
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
      DATYPE = '  '
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   DATYPE, NLUSER, STAT, BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1010) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
         END IF
      CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1015) IERR
         GO TO 990
         END IF
C                                       header ext format
      CALL FXHDEX (CATBLK)
C                                       Put new values in CATBLK.
      CALL MAKOUT (NAMEIN, CLAIN, SEQIN, BLANK, NAMOUT, CLAOUT, SEQOUT)
      CALL CHR2H (12, NAMOUT, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLAOUT, KHIMCO, CATH(KHIMC))
      CATBLK(KIIMS) = SEQOUT
C                                       Create output file.
      CCNO = 1
      JERR = 4
C                                       Output is dummy UV file
      IF (DATYPE.EQ.'UV') THEN
         CALL UVPGET (JERR)
         IF (JERR.NE.0) GO TO 999
C                                       with 1 visibility
         CATBLK(KIGCN) = 1
         CALL UVCREA (DISKO, CCNO, BUFF1, IERR)
C                                       Image file instead
      ELSE
C                                       preserve coordinates
         DO 30 I = 1,7
            J = IROUND (CATR(KRCRP+I-1))
            J = MAX (1, MIN (J, CATBLK(KINAX+I-1)))
            BLC(I) = J
            TRC(I) = J
 30         CONTINUE
         XINC = 1.0
         YINC = 1.0
         CALL SUBHDR (BLC, TRC, XINC, YINC)
         CALL MCREAT (DISKO, CCNO, BUFF1, IERR)
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1050) IERR, DATYPE
         GO TO 990
         END IF
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKO
      FCNO(NCFILE) = CCNO
      FRW(NCFILE) = 2
C                                        Put input file in READ
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN,
     *   DATYPE, NLUSER, 'READ', BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
      SEQOUT = CATBLK(KIIMS)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TBLSIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1010 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1015 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT ',A2,' FILE')
      END
      SUBROUTINE TBSVHI
C-----------------------------------------------------------------------
C   TBSVHI copies and updates history file.  It also copies any tables.
C-----------------------------------------------------------------------
      CHARACTER NOTTYP*2, HILINE*72
      INTEGER   LUN1, LUN2, IERR, NONOT
      INCLUDE 'TASAV.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHIS.INC'
      INCLUDE 'INCS:DCAT.INC'
      DATA LUN1, LUN2 /27, 28/
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
C                                       Add history entry
      WRITE (HILINE,1010) TSKNAM
      CALL HIADD (LUN2, HILINE, BUFF2, IERR)
C                                       Close HI file
 200  CALL HICLOS (LUN2, .TRUE., BUFF2, IERR)
C                                       Copy tables
      CALL ALLTAB (NONOT, NOTTYP, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF (IERR.GT.2) THEN
         WRITE (MSGTXT,1200)
         CALL MSGWRT (6)
         END IF
C                                       Copy PL and SL
      CALL FILCOP ('PL', 0, 0, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) THEN
         WRITE (MSGTXT,1210) IERR, 'PL'
         CALL MSGWRT (6)
         END IF
      CALL FILCOP ('SL', 0, 0, LUN1, LUN2, DISKIN, DISKO, FCNO(2),
     *   FCNO(1), CATBLK, BUFF1, BUFF2, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.2)) THEN
         WRITE (MSGTXT,1210) IERR, 'SL'
         CALL MSGWRT (6)
         END IF
C                                       Update CATBLK.
      CALL CATIO ('UPDT', DISKO, FCNO(NCFILE-1), CATBLK, 'REST',
     *   BUFF1, IERR)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('TBSVHI: ERROR',I3,' COPY/OPEN HISTORY FILE')
 1010 FORMAT (A6,' / I copied all extension tables')
 1200 FORMAT ('TBSVHI: ERROR COPYING TABLES')
 1210 FORMAT ('TBSVHI: ERROR',I4,' COPYING ',A2,' FILES')
      END
