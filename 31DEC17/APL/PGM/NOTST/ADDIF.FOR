      PROGRAM ADDIF
C-----------------------------------------------------------------------
C! Task to add an IF axis to the header of a uv-file.
C# Catalogue Header UV-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   ADDIF modifies the catalog header of a uv-data base by adding
C   an IF axis, this can be missing from 'old' data and is needed
C   when DBCONing such data to 'new' or AIPS calibrated data.
C   Inputs:
C      AIPS adverb  Prg. name.          Description.
C      INNAME         NAMEIN        Name of input UV data.
C      INCLASS        CLAIN         Class of input UV data.
C      INSEQ          SEQIN         Seq. of input UV data.
C      INDISK         DISKIN        Disk number of input VU data.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6
      INTEGER  SEQIN, DISKIN, JBUFSZ, IRET
      HOLLERITH XNAMEI(3), XCLAIN(2)
      CHARACTER NAMEIN*12, CLAIN*6
      REAL      XSIN, XDISIN, BUFF1(1600)
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN,
     *   SEQIN, DISKIN
      COMMON /CHPARM/ NAMEIN, CLAIN
C
      DATA PRGM /'ADDIF '/
C-----------------------------------------------------------------------
C                                       Get input parameters and
C                                       insert IF in header
      CALL ADIFIN (PRGM, IRET)
      IF (IRET.NE.0) GO TO 990
C                                       Close down files, etc.
 990  CALL DIE (IRET, BUFF1)
C
 999  STOP
      END
      SUBROUTINE ADIFIN (PRGN, JERR)
C-----------------------------------------------------------------------
C   ADIFIN gets input parameters for ADDIF.
C   Inputs:  PRGN    C*6)      Program name (2 chars/word)
C   Output:  JERR    I         Error code: 0 => ok
C                                5 => catalog troubles
C                                8 => can't start
C   Commons: /INPARM/ all input adverbs in order given by INPUTS
C                     file
C            /MAPHDR/ output file catalog header
C
C-----------------------------------------------------------------------
      CHARACTER  NAMEIN*12, CLAIN*6, STAT*4, PRGN*6, UTYPE*2
      HOLLERITH  XNAMEI(3), XCLAIN(2)
      INTEGER  JERR
      INTEGER   OLDCNO, SEQIN, DISKIN, JBUFSZ, IROUND, NPARM, IERR
      REAL      XSIN, XDISIN, BUFF1(1600)
      LOGICAL   T, F
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DCAT.INC'
      COMMON /BUFRS/ BUFF1, JBUFSZ
      COMMON /INPARM/ XNAMEI, XCLAIN, XSIN, XDISIN, SEQIN, DISKIN
      COMMON /CHPARM/ NAMEIN, CLAIN
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Init for AIPS, disks, ...
      TSKNAM = ' '
      CALL ZDCHIN (.TRUE., BUFF1)
      CALL VHDRIN
      JBUFSZ = 3200
C                                       Initialize /CFILES/
      NSCR = 0
      NCFILE = 0
      JERR = 0
C                                       Get input parameters.
      NPARM = 7
      CALL GTPARM (PRGN, NPARM, RQUICK, XNAMEI, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 10
         RQUICK = .TRUE.
         JERR = 8
         IF (IERR.EQ.1) GO TO 999
            WRITE (MSGTXT,1000) IERR
            CALL MSGWRT (8)
C                                       Restart AIPS
 10   IF (RQUICK) CALL RELPOP (JERR, BUFF1, IERR)
      IF (JERR.NE.0) GO TO 999
      JERR = 5
C                                       Crunch input parameters.
 20   SEQIN = IROUND (XSIN)
      DISKIN = IROUND (XDISIN)
      CALL H2CHR (12, 1, XNAMEI, NAMEIN)
      CALL H2CHR (6, 1, XCLAIN, CLAIN)
C                                       Get CATBLK from old file.
 30   OLDCNO = 1
      UTYPE = 'UV'
      CALL CATDIR ('SRCH', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 40
         WRITE (MSGTXT,1030) IERR, NAMEIN, CLAIN, SEQIN, DISKIN,
     *      NLUSER
         GO TO 990
 40   CALL CATIO ('READ', DISKIN, OLDCNO, CATBLK, 'REST', BUFF1, IERR)
      IF (IERR.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IERR
         GO TO 990
C                                       Get uv header info.
 50   CALL UVPGET (JERR)
      IF (JERR.NE.0) GO TO 999
C                                       Add IF axis to catblk
      CALL CADDIF (CATBLK, IERR)
      IF (IERR.NE.0) GO TO 999
C                                        Put input file in WRIT
      UTYPE = 'UV'
      STAT = 'WRIT'
      CALL CATDIR ('CSTA', DISKIN, OLDCNO, NAMEIN, CLAIN, SEQIN, UTYPE,
     *   NLUSER, STAT, BUFF1, IERR)
      NCFILE = NCFILE + 1
      FVOL(NCFILE) = DISKIN
      FCNO(NCFILE) = OLDCNO
      FRW(NCFILE) = 0
      JERR = 0
C                                        Update CATBLK.
      CALL CATIO ('UPDT', DISKIN, FCNO(NCFILE), CATBLK, 'REST', BUFF1,
     *   IERR)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('ADIFIN: ERROR',I3,' OBTAINING INPUT PARAMETERS')
 1030 FORMAT ('ERROR',I3,' FINDING ',A12,'.',A6,'.',I4,' DISK=',
     *   I3,' USID=',I5)
 1040 FORMAT ('ERROR',I3,' COPYING CATBLK ')
 1050 FORMAT ('ERROR',I3,' CREATING OUTPUT FILE')
 1060 FORMAT ('MAY OVERWRITE INPUT FILE ONLY.  QUITTING')
 1065 FORMAT ('ADIFIN: ERROR',I3,' UPDATING NEW CATBLK')
      END
      SUBROUTINE CADDIF (IDUM, IERR)
C-----------------------------------------------------------------------
C Routine to add an IF axis to the catblk header
C-----------------------------------------------------------------------
      CHARACTER TYPES(7)*8
      INTEGER   I, INDEX, INC, IERR, NAXIS, IDUM(1)
      LOGICAL   EXIST
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
      CALL COPY (256, IDUM, CATBLK)
      EXIST = .FALSE.
      IERR = 0
      NAXIS = CATBLK(KIDIM)
C                                       Obtain random axes names
C                                       & check if IF already there
      INC = 2
      DO 10 I = 1, NAXIS
         INDEX = KHCTP + (I-1) * INC
         CALL H2CHR (8, 1, CATH(INDEX), TYPES(I))
         IF (TYPES(I).EQ.'IF      ') EXIST = .TRUE.
 10      CONTINUE
C                                       Does it exist?
      IF (EXIST) THEN
         WRITE (MSGTXT,1000)
         IERR = 1
         GO TO 990
         END IF
C                                       Create IF axis
      I = NAXIS + 1
      CATBLK (KINAX+I-1) = 1
      CATR(KRCIC+I-1) = 1.0
      CATR(KRCRT+I-1) = 0.0
      CATR(KRCRP+I-1) = 1.0
      CATD(KDCRV+I-1) = 1.0D0
      INDEX = KHCTP + (I-1) * INC
      CALL CHR2H (8, 'IF      ', 1, CATH(INDEX))
      CATBLK(KIDIM) = NAXIS + 1
      CALL COPY (256, CATBLK, IDUM)
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('IF AXIS ALREADY EXISTS - THIS OPERATION IS POINTLESS')
C-----------------------------------------------------------------------
      END
