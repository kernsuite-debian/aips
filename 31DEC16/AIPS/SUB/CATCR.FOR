      SUBROUTINE CATCR (IVOL, IMAX, BUFFER, IERR)
C-----------------------------------------------------------------------
C! Create and initialize catalog (CA) files
C# Catalog
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997
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
C   CATCR creates and initializes catalog files ('CA').  If the file
C   already exists, nothing is done and no error is generated.
C   CATCR also will create and initialize the user's message file.
C   Inputs:
C      IVOL    I       Disk volume (0 => all)
C      IMAX    I       Min # entries in catalog (>0 ONLY)
C   Output:
C      BUFFER  I(256)  Scratch buffer
C      IERR    I       Number of disks for which there was some error
C-----------------------------------------------------------------------
      INTEGER   IVOL, IMAX, BUFFER(256), IERR
C
      CHARACTER PHNAME*48
      INTEGER   IER, NWPL, NLPR, I, NREC, IX, IV, IV1, IV2, LUN, FIND,
     *   IRNO, ISIZE, LSIZE
      LOGICAL   T, F, IAMOK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA T, F /.TRUE.,.FALSE./
      DATA LUN /15/
C-----------------------------------------------------------------------
      IERR = 0
C                                       Message file
 10   CONTINUE
C                                       file size
         NWPL = 25
         NLPR = 255 / NWPL
         ISIZE = 25/NLPR + 1
         CALL ZPHFIL ('MS', 1, NLUSER, 0, PHNAME, IER)
         CALL ZEXIST (1, PHNAME, LSIZE, IER)
         IF (IER.EQ.0) GO TO 100
         CALL ZCREAT (1, PHNAME, ISIZE, F, LSIZE, IER)
         IF (IER.EQ.1) GO TO 100
            IF (IER.EQ.0) GO TO 20
               IERR = IERR + 10
               WRITE (MSGTXT,1010) IER
               CALL MSGWRT (7)
               GO TO 100
 20         CALL ZOPEN (LUN, FIND, 1, PHNAME, F, T, T, IER)
            IF (IER.EQ.0) GO TO 30
               IERR = IERR + 10
               WRITE (MSGTXT,1020) IER
               CALL MSGWRT (7)
               GO TO 100
 30         CALL FILL (256, 0, BUFFER)
            NREC = LSIZE
            BUFFER(1) = 0
            BUFFER(2) = NREC
            IRNO = 1
            CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IER)
            IF (IER.EQ.0) GO TO 40
               IERR = IERR + 10
               WRITE (MSGTXT,1030) IER
               CALL MSGWRT (7)
 40         CALL ZCLOSE (LUN, FIND, IER)
C                                       CATAlog files
 100  IF (IMAX.LE.0) GO TO 999
C                                       file size
         NWPL = 10
         NLPR = 256 / NWPL
         ISIZE = 2 + (IMAX-1)/NLPR
C                                       Disks to do
         IV1 = IVOL
         IV2 = IVOL
         IF ((IVOL.GT.0) .AND. (IVOL.LE.NVOL)) GO TO 110
            IV1 = 1
            IV2 = NVOL
C                                       Loop on disks
 110     DO 170 IV = IV1,IV2
            IF (IV1.LT.IV2) THEN
               IF (.NOT.IAMOK(IV,'CA')) GO TO 170
               END IF
            CALL ZPHFIL ('CA', IV, 0, 0, PHNAME, IER)
            CALL ZEXIST (IV, PHNAME, LSIZE, IER)
            IF (IER.EQ.0) GO TO 170
            CALL ZCREAT (IV, PHNAME, ISIZE, F, LSIZE, IER)
            IF (IER.EQ.1) GO TO 170
               IF (IER.EQ.0) GO TO 115
                  IERR = IERR + 1
                  WRITE (MSGTXT,1110) IV, IER
                  CALL MSGWRT (7)
                  GO TO 170
 115           CALL ZOPEN (LUN, FIND, IV, PHNAME, F, T, T, IER)
               IF (IER.EQ.0) GO TO 120
                  IERR = IERR + 1
                  WRITE (MSGTXT,1115) IV, IER
                  CALL MSGWRT (7)
                  GO TO 170
 120           CALL FILL (256, 0, BUFFER)
C                                       How many will it actually hold
               NREC = LSIZE
               IX = (NREC - 1) * NLPR
               IF (IX.GT.46655) IX = 46655
               BUFFER(1) = IV
               BUFFER(3) = IX
               CALL ZDATE (BUFFER(4))
               CALL ZTIME (BUFFER(7))
               CALL COPY (6, BUFFER(4), BUFFER(10))
               IRNO = 1
               CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IER)
               IF (IER.EQ.0) GO TO 145
                  IERR = IERR + 1
                  WRITE (MSGTXT,1140) IV, 1, IER
                  CALL MSGWRT (7)
                  GO TO 160
 145           CALL FILL (20, 0, BUFFER)
               DO 150 I = 1,256,NWPL
                  BUFFER(I) = -1
 150              CONTINUE
               DO 155 I = 2,NREC
                  IRNO = I
                  CALL ZFIO ('WRIT', LUN, FIND, IRNO, BUFFER, IER)
                  IF (IER.EQ.0) GO TO 155
                     IERR = IERR + 1
                     WRITE (MSGTXT,1140) IV, I, IER
                     CALL MSGWRT (7)
                     GO TO 160
 155              CONTINUE
 160           CALL ZCLOSE (LUN, FIND, IER)
 170        CONTINUE
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('CATCR: CREATE MESSAGE FILE ERROR',I7)
 1020 FORMAT ('CATCR: OPEN MESSAGE FILE ERROR',I7)
 1030 FORMAT ('CATCR: WRITE MESSAGE FILE REC 1: ERROR',I7)
 1110 FORMAT ('CATCR: CREATE CATLG ON VOL',I2,' ERROR',I7)
 1115 FORMAT ('CATCR: OPEN CATLG ON VOL',I2,' ERROR',I7)
 1140 FORMAT ('CATCR: WRITE CATLG ON VOL',I2,' REC',I4,' ERROR',I7)
      END
