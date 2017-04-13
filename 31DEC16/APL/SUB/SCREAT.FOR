      SUBROUTINE SCREAT (SIZE, WBUFF, IERR)
C-----------------------------------------------------------------------
C! create an AIPS-standard scratch file w common DFIL.INC, ...
C# IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2004, 2012
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
C   SCREAT is intended to replace all previous scratch file creation
C   routines in AIPS (beginning on February 11, 1985).  It uses the
C   Common included via the new DFIL.INC, CFIL.INC pair and returns
C   the scratch file disk and catalog number in variables SCRVOL(NSCR)
C   and SCRCNO(NSCR), where NSCR is updated on successful creation.
C   It attempts to avoid the disk used for the previously created
C   scratch file.  All files have physical name SCvccc01 where v is the
C   disk number and ccc is the catalog slot number.  Their logical
C   names are determined from the routine BLDSNM.
C
C   SCREAT was modified to insert the system name in the catalog
C   header so that the destroy routines will not destroy some other
C   machines scratch files.
C
C   Input:
C      SIZE    I          Desired size in AIPS blocks (256-integer
C                         blocks)
C   Output:
C      WBUFF   I(512)     Scratch buffer    (NOTE 512 integers)
C      IERR    I          error: 0 => ok
C                            1 => catalog error in setting name
C                            2 => catalog error on open
C                            3 => CATIO error writing header to catlg
C                            4 => No allowed disk with room
C   Commons:
C      /MAPHDR/   in      scratch file image header - contents mostly
C                         ignored
C      /CFILES/   in/out  file info
C   Note: this common uses IBAD to specify BADDISKs which are avoided.
C-----------------------------------------------------------------------
      INTEGER   SIZE, WBUFF(512), IERR
C
      CHARACTER NAME*12, CLASS*6, STAT*4, PHNAME*48, TYPTMP*2
      CHARACTER SYSKYS(2)*8
      HOLLERITH NAMESY(4)
      INTEGER   NLOCS(2), NKEYTP(2), ASIZE, SNO, IVOL, CNO, I, IV,
     *   NTRY, ITRY(15)
      LOGICAL   MAP, IAMOK
      INCLUDE 'INCS:DFIL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DCAT.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DDRC.INC'
      DATA MAP /.TRUE./
      DATA NLOCS /1, 3/, NKEYTP /3, 3/
      DATA SYSKYS /'SYSNAME1', 'SYSNAME2'/
C-----------------------------------------------------------------------
      IF (NSCR.GE.SCRLST) THEN
         MSGTXT = 'SCREAT: SCRATCH FILE LIST OVERFLOWS'
         CALL MSGWRT (8)
         IERR = 8
         GO TO 999
         END IF
C                                       Build logical name
      CALL BLDSNM (TSKNAM, NPOPS, NAME, CLASS, IERR)
C                                       protect current mapheader
      CALL COPY (256, CATBLK, WBUFF(257))
C                                       scratch header
      CATBLK(KIDIM) = 0
      CATBLK(KIPCN) = 0
      CATBLK(KIIMS) = 0
      CALL CATCLR (CATBLK)
      CALL CHR2H (12, NAME, KHIMNO, CATH(KHIMN))
      CALL CHR2H (6, CLASS, KHIMCO, CATH(KHIMC))
      CALL CHR2H (2, 'SC', KHPTYO, CATH(KHPTY))
      CALL ZDATE (WBUFF)
      WRITE (MSGTXT,1000) WBUFF(1), WBUFF(2), WBUFF(3)
      CALL CHR2H (8, MSGTXT, 1, CATH(KHDMP))
C                                       prepare the system name
C                                       20 characters go into
C                                       3 8 char keywords.
      CALL CHR2H (8, HSTNAM(1:8), 1, NAMESY(1))
      CALL CHR2H (8, HSTNAM(9:), 1, NAMESY(3))
C                                       find highest seq number + 1
      SNO = 0
      CNO = 1
      IVOL = 0
      TYPTMP = ' '
      CALL CATDIR ('SRCH', IVOL, CNO, NAME, CLASS, SNO, TYPTMP, NLUSER,
     *   STAT, WBUFF, IERR)
      IF ((IERR.NE.0) .AND. (IERR.NE.5)) THEN
         WRITE (MSGTXT,1010) IERR, 'SEARCHING CATALOG TO MAKE SCRATCH'
         IERR = 1
         GO TO 980
         END IF
      IF (IERR.EQ.5) SNO = 0
      SNO = SNO + 1
      CATBLK(KIIMS) = SNO
C                                       set pointers to try
      IV = 1
      IF (NSCR.GE.1) IV = SCRVOL(NSCR)
      IF ((NSCR.LE.0) .AND. (NCFILE.GE.1)) IV = FVOL(NCFILE)
C                                       loop over disks
      NTRY = 0
      DO 50 IVOL = 1,NVOL
C                                       select next volume
         IV = MOD (IV, NVOL) + 1
         DO 15 I = 1,10
            IF (IV.EQ.IBAD(I)) GO TO 50
 15         CONTINUE
         IF (.NOT.IAMOK(IV,'SC')) GO TO 50
         IF (DSKLOK(IVOL).EQ.1) GO TO 50
         IF (NTRY.LT.15) THEN
            NTRY = NTRY + 1
            ITRY(NTRY) = IV
            END IF
C                                       create a catalog slot
         CNO = 1
         TYPTMP = 'SC'
         CALL CATDIR ('OPEN', IV, CNO, NAME, CLASS, SNO, TYPTMP, NLUSER,
     *       STAT, WBUFF, IERR)
         IF (IERR.NE.0) THEN
            IF ((IERR.EQ.1) .OR. (IERR.EQ.5)) THEN
               WRITE (MSGTXT,1015) IV
               MSGTXT(25:) = 'could not open catalog file'
               IF (IERR.EQ.5) MSGTXT(25:) =
     *            'no room in the catalog file'
               CALL MSGWRT (6)
               GO TO 50
            ELSE
               WRITE (MSGTXT,1010) IERR, 'OPENING CATALOG SCRATCH SLOT'
               IERR = 2
               GO TO 980
               END IF
            END IF
C                                       header to disk
         CALL CATIO ('UPDT', IV, CNO, CATBLK, 'REST', WBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR,
     *         'WRITING SCRATCH HEADER TO CATALOG'
            IERR = 3
            GO TO 970
            END IF
C                                       system name keywords to disk
         CALL CATKEY ('WRIT', IV, CNO, SYSKYS, 2, NLOCS, NAMESY, NKEYTP,
     *      WBUFF, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1010) IERR, 'WRITING SYSNAME CATALOG HEADER'
            IERR = 4
            GO TO 970
            END IF
C                                       create
         CALL ZPHFIL ('SC', IV, CNO, 1, PHNAME, IERR)
         CALL ZCREAT (IV, PHNAME, SIZE, MAP, ASIZE, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1040) IV, IERR
            IF (IERR.EQ.3) WRITE (MSGTXT,1041) IV, SIZE
            IF (IERR.EQ.5) WRITE (MSGTXT,1042) IV
            CALL MSGWRT (8)
C                                       clean up cat - status, slot
            TYPTMP = 'SC'
            CALL CATDIR ('CSTA', IV, CNO, NAME, CLASS, SNO, TYPTMP,
     *         NLUSER, 'CLWR', WBUFF, IERR)
            TYPTMP = 'SC'
            CALL CATDIR ('CLOS', IV, CNO, NAME, CLASS, SNO, TYPTMP,
     *         NLUSER, STAT, WBUFF, IERR)
         ELSE
            GO TO 60
            END IF
 50      CONTINUE
C                                       cannot do it
      MSGTXT = 'SCREAT: FAILED TO CREATE REQUESTED SCRATCH FILE'
      CALL MSGWRT (8)
      MSGTXT = 'SCREAT: PERHAPS BADDISK IS TOO EXTENSIVE'
      CALL MSGWRT (8)
      WRITE (MSGTXT,1050) (ITRY(I), I = 1,NTRY)
      IERR = 5
      GO TO 980
C                                       Did it
 60   NSCR = NSCR + 1
      SCRVOL(NSCR) = IV
      SCRCNO(NSCR) = CNO
      GO TO 990
C                                       Error returns
C                                       clean up cat - status, slot
 970  CALL MSGWRT (8)
      TYPTMP = 'SC'
      CALL CATDIR ('CSTA', IV, CNO, NAME, CLASS, SNO, TYPTMP, NLUSER,
     *   'CLWR', WBUFF, IERR)
      TYPTMP = 'SC'
      CALL CATDIR ('CLOS', IV, CNO, NAME, CLASS, SNO, TYPTMP, NLUSER,
     *   STAT, WBUFF, IERR)
      GO TO 990
C                                       No clean up
 980  CALL MSGWRT (8)
C                                       restore header
 990  CALL COPY (256, WBUFF(257), CATBLK)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT (I4.4,I2.2,I2.2)
 1010 FORMAT ('SCREAT: ERROR',I5,' ON ',A)
 1015 FORMAT ('SCREAT: DISK',I3,' WARNING')
 1040 FORMAT ('SCREAT: DISK',I3,' ERROR',I5,' ON SCRATCH CREATE')
 1041 FORMAT ('SCREAT: NO ROOM ON',I3,' FOR SCRATCH SIZE',I10,
     *   ' KILOBYTES')
 1042 FORMAT ('SCREAT: PROHIBITED FROM USING DISK ',I3)
 1050 FORMAT ('SCREAT: TRIED DISKS',15I3)
      END
