      PROGRAM DRCHK
C-----------------------------------------------------------------------
C! Intercompares HOSTS.LIST, DADEVS.LIST, NETSP, and $NET0
C# Utility
C-----------------------------------------------------------------------
C;  Copyright (C) 2001, 2009, 2012
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
C   DRCHK checks the HOSTS.LIST list against NETSP, DADEVS, and the
C   directories in the $NET0 area, reporting any inconsistencies.
C-----------------------------------------------------------------------
      INTEGER   MHOST, NETSP, MSITE
      PARAMETER (MHOST=500, NETSP=8192, MSITE=6)
      CHARACTER PRGNAM*8, HOSTS(MHOST,MSITE)*16, DISKS(NETSP)*64,
     *   LINE*80, LFILE*80, THOST*16, SITES(MSITE)*8, TSITE*8, TDISK*64,
     *   AHOST*18, MSGTMP*160, SFILE*80, XLATED*80
      INTEGER   LUN, I, J, K, NHOST(MSITE), NHUSE(MHOST,MSITE), II, JJ,
     *   NNUSE(NETSP), IND, NSITE, NDISK, DSITE, SLUN, SIND, XLNB,
     *   IERR, KBP, JTRIM
      INCLUDE 'INCS:DMSG.INC'
      DATA PRGNAM /'DRCHK'/
      DATA LUN, SLUN /11,10/
C-----------------------------------------------------------------------
      TSKNAM = PRGNAM
      NPOPS = 1
      NLUSER = 1
      CALL ZDCHIN (.FALSE.)
C                                       Read HOSTS.LIST
      NSITE = 0
      CALL FILL (MSITE, 0, NHOST)
      I = MHOST * MSITE
      CALL FILL (I, 0, NHUSE)
      LFILE = 'AIPS_ROOT:HOSTS.LIST'
      CALL ZTXOPN ('READ', LUN, IND, LFILE, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Read loop
 10   CONTINUE
         CALL ZTXIO ('READ', LUN, IND, LINE, IERR)
         IF (IERR.EQ.0) THEN
            I = JTRIM (LINE)
C                                       get Host name and site
            IF (LINE(1:1).EQ.'+') THEN
               KBP = 4
               CALL NXTSTR (LINE, 80, KBP, THOST, K)
               CALL NXTSTR (LINE, 80, KBP, TSITE, K)
               CALL NXTSTR (LINE, 80, KBP, TSITE, K)
C              CALL CHLTOU (16, THOST)
C              CALL CHLTOU (8, TSITE)
C                                       already mentioned
               DO 20 I = 1,NSITE
                  DO 15 J = 1,NHOST(I)
                     IF (THOST.EQ.HOSTS(J,I)) THEN
                        IF (TSITE.EQ.SITES(I)) THEN
                           WRITE (MSGTXT,1010) THOST, TSITE
                        ELSE
                           WRITE (MSGTXT,1011) THOST, TSITE, SITES(I)
                           END IF
                        CALL MSGWRT (6)
                        GO TO 10
                        END IF
 15                  CONTINUE
 20               CONTINUE
C                                       old site, new host
               DO 30 I = 1,NSITE
                  IF (TSITE.EQ.SITES(I)) THEN
                     NHOST(I) = NHOST(I) + 1
                     HOSTS(NHOST(I),I) = THOST
                     GO TO 10
                     END IF
 30               CONTINUE
C                                       new site and host
               NSITE = NSITE + 1
               NHOST(NSITE) = 1
               HOSTS(1,NSITE) = THOST
               SITES(NSITE) = TSITE
               END IF
            GO TO 10
C                                       I/O error
         ELSE IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1030) LFILE, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       end reading HOSTS.LIST
      CALL ZTXCLS (LUN, IND, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Read NETSP file
      NDISK = 0
      CALL FILL (NETSP, 0, NNUSE)
      LFILE = 'NET0:NETSP'
      CALL ZTXOPN ('READ', LUN, IND, LFILE, .FALSE., IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Read loop
 50   CONTINUE
         CALL ZTXIO ('READ', LUN, IND, LINE, IERR)
         IF (IERR.EQ.0) THEN
            I = JTRIM (LINE)
C                                       Non-comment: get disk name
            IF ((LINE.NE.' ') .AND. (LINE(1:1).NE.'#')) THEN
               KBP = 1
               CALL NXTSTR (LINE, 80, KBP, TDISK, K)
C              CALL CHLTOU (64, TDISK)
C                                       already defined
               DO 60 I = 1,NDISK
                  IF (TDISK.EQ.DISKS(I)) THEN
                     WRITE (MSGTXT,1050) TDISK
                     CALL MSGWRT (6)
                     GO TO 50
                     END IF
 60               CONTINUE
               NDISK = NDISK + 1
               DISKS(NDISK) = TDISK
               END IF
            GO TO 50
C                                       I/O error
         ELSE IF (IERR.NE.2) THEN
            WRITE (MSGTXT,1030) LFILE, IERR
            CALL MSGWRT (8)
            GO TO 999
            END IF
C                                       end reading NETSP
      CALL ZTXCLS (LUN, IND, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Check DADEVS.LIST.$SITE files
      DSITE = 0
      DO 200 I = 1,NSITE
         LFILE = 'NET0:DADEVS.LIST.' // SITES(I)
         MSGSUP = 32000
         CALL ZTXOPN ('QRED', LUN, IND, LFILE, .FALSE., IERR)
         MSGSUP = 0
C                                       file exists: read
         IF (IERR.EQ.0) THEN
            DSITE = DSITE + 1
            MSGTXT = 'Checking DADEVS.LIST.' // SITES(I)
            CALL MSGWRT (4)
 110        CALL ZTXIO ('READ', LUN, IND, LINE, IERR)
            IF ((IERR.EQ.0) .AND. (LINE(1:1).NE.'#') .AND.
     *         (LINE.NE.' ')) THEN
               J = JTRIM (LINE)
               TDISK = LINE(4:)
C              CALL CHLTOU (64, TDISK)
C                                       find HOST attached
               THOST = ' '
               DO 120 J = 1,NSITE
                  DO 115 K = 1,NHOST(J)
                     JJ = JTRIM (HOSTS(K,J))
                     AHOST = HOSTS(K,J)(:JJ) // '_'
                     II = INDEX (TDISK, AHOST(:JJ+1))
                     IF (II.GT.0) THEN
                        NHUSE(K,J) = NHUSE(K,J) + 1
                        IF (THOST.EQ.' ') THEN
                           THOST = HOSTS(K,J)
                        ELSE
                           WRITE (MSGTMP,1110) THOST, AHOST, TDISK
                           CALL REFRMT (MSGTMP, '_', II)
                           MSGTXT = MSGTMP
                           CALL MSGWRT (6)
                           END IF
                        END IF
 115                 CONTINUE
 120              CONTINUE
               IF (THOST.EQ.' ') THEN
                  WRITE (MSGTXT,1120) TDISK
                  CALL MSGWRT (6)
                  END IF
C                                       Open SPACE file to test
               JJ = JTRIM (TDISK)
               SFILE = TDISK(:JJ) // '/SPACE'
               MSGTXT = 'Opening SPACE file on ' // TDISK
               CALL MSGWRT (1)
               CALL ZTXOPN ('QRED', SLUN, SIND, SFILE, .FALSE., IERR)
               IF (IERR.EQ.0) THEN
                  CALL ZTXCLS (SLUN, SIND, IERR)
               ELSE
                  MSGTXT = 'Open SPACE failed on ' // TDISK
                  CALL MSGWRT (8)
                  END IF
C                                       Is it in NETSP
               JJ = 0
               DO 130 J = 1,NDISK
                  IF (TDISK.EQ.DISKS(J)) THEN
                     NNUSE(J) = NNUSE(J) + 1
                     JJ = J
                     END IF
 130              CONTINUE
               IF (JJ.LE.0) THEN
                  MSGTXT = 'NETSP DOES NOT CONTAIN ' // TDISK
                  CALL MSGWRT (7)
                  END IF
               GO TO 110
               END IF
            IF (IERR.EQ.0) THEN
               GO TO 110
            ELSE IF (IERR.NE.2) THEN
               WRITE (MSGTXT,1030) LFILE, IERR
               CALL MSGWRT (7)
               GO TO 999
               END IF
C                                       end reading DADEVS.LIST.site
            CALL ZTXCLS (LUN, IND, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
 200     CONTINUE
C                                       Check DADEVS.LIST files
      IF (DSITE.LT.NSITE) THEN
         LFILE = 'NET0:DADEVS.LIST'
         CALL ZTXOPN ('QRED', LUN, IND, LFILE, .FALSE., IERR)
C                                       file exists: read
         IF (IERR.EQ.0) THEN
            MSGTXT = 'Checking DADEVS.LIST'
            CALL MSGWRT (4)
 210        CALL ZTXIO ('READ', LUN, IND, LINE, IERR)
            IF ((IERR.EQ.0) .AND. (LINE(1:1).NE.'#') .AND.
     *         (LINE.NE.' ')) THEN
               J = JTRIM (LINE)
               TDISK = LINE(4:)
C              CALL CHLTOU (64, TDISK)
C                                       find HOST attached
               THOST = ' '
               DO 220 J = 1,NSITE
                  DO 215 K = 1,NHOST(J)
                     JJ = JTRIM (HOSTS(K,J))
                     AHOST = HOSTS(K,J)(:JJ) // '_'
                     II = INDEX (TDISK, AHOST(:JJ+1))
                     IF (II.GT.0) THEN
                        NHUSE(K,J) = NHUSE(K,J) + 1
                        IF (THOST.EQ.' ') THEN
                           THOST = HOSTS(K,J)
                        ELSE
                           WRITE (MSGTMP,1110) THOST, AHOST, TDISK
                           CALL REFRMT (MSGTMP, '_', II)
                           MSGTXT = MSGTMP
                           CALL MSGWRT (6)
                           END IF
                        END IF
 215                 CONTINUE
 220              CONTINUE
               IF (THOST.EQ.' ') THEN
                  WRITE (MSGTXT,1120) TDISK
                  CALL MSGWRT (6)
                  END IF
C                                       Open SPACE file to test
               JJ = JTRIM (TDISK)
               SFILE = TDISK(:JJ) // '/SPACE'
               MSGTXT = 'Opening SPACE file on ' // TDISK
               CALL MSGWRT (1)
               CALL ZTXOPN ('QRED', SLUN, SIND, SFILE, .FALSE., IERR)
               IF (IERR.EQ.0) THEN
                  CALL ZTXCLS (SLUN, SIND, IERR)
               ELSE
                  MSGTXT = 'Open SPACE failed on ' // TDISK
                  CALL MSGWRT (8)
                  END IF
C                                       Is it in NETSP
               JJ = 0
               DO 230 J = 1,NDISK
                  IF (TDISK.EQ.DISKS(J)) THEN
                     NNUSE(J) = NNUSE(J) + 1
                     JJ = J
                     END IF
 230              CONTINUE
               IF (JJ.LE.0) THEN
                  MSGTXT = 'NETSP DOES NOT CONTAIN ' // TDISK
                  CALL MSGWRT (7)
                  END IF
               GO TO 210
               END IF
            IF (IERR.EQ.0) THEN
               GO TO 210
            ELSE IF (IERR.NE.2) THEN
               WRITE (MSGTXT,1030) LFILE, IERR
               CALL MSGWRT (7)
               GO TO 999
               END IF
C                                       end reading DADEVS.LIST
            CALL ZTXCLS (LUN, IND, IERR)
            IF (IERR.NE.0) GO TO 999
            END IF
         END IF
C                                       was NETSP overused, underused?
      DO 300 I = 1,NDISK
         IF (NNUSE(I).LE.0) THEN
            MSGTXT = 'NETSP entry unused: ' // DISKS(I)
            CALL MSGWRT (6)
         ELSE IF (NNUSE(I).GT.1) THEN
            JJ = JTRIM (DISKS(I))
            JJ = MIN (JJ, 53)
            WRITE (MSGTXT,1300) NNUSE(I), DISKS(I)(:JJ)
            CALL MSGWRT (6)
            END IF
 300     CONTINUE
C                                       Was a host unused?
      DO 320 J = 1,NSITE
         DO 315 K = 1,NHOST(J)
            JJ = JTRIM (HOSTS(K,J))
            JJ = MIN (JJ, 40)
            IF (NHUSE(K,J).LE.0) THEN
               WRITE (MSGTXT,1310) HOSTS(K,J)(:JJ)
               CALL MSGWRT (6)
               END IF
 315        CONTINUE
 320     CONTINUE
C                                       Do $NET0/HOST/ files exist?
      CALL ZTRLOG (4, 'NET0', 80, XLATED, XLNB, IERR)
      IF (IERR.NE.0) THEN
         MSGTXT = 'UNABLE TO TRANSLATE NET0'
         CALL MSGWRT (8)
         GO TO 999
         END IF
      DO 400 I = 1,NSITE
         DO 390 J = 1,NHOST(I)
            SFILE = XLATED(:XLNB) // '/' // HOSTS(J,I)
            JJ = JTRIM (SFILE)
            SFILE(JJ+1:) =  '/ACD000000\\;'
            MSGTXT = 'Opening ' // SFILE
            CALL MSGWRT (1)
            CALL ZTXOPN ('QRED', SLUN, SIND, SFILE, .FALSE., IERR)
            IF (IERR.EQ.0) THEN
               CALL ZTXCLS (SLUN, SIND, IERR)
            ELSE
               MSGTXT = 'Open AC failed on ' // HOSTS(J,I)
               CALL MSGWRT (8)
               END IF
 390        CONTINUE
 400     CONTINUE
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT (A,' repeated for site ',A)
 1011 FORMAT (A,' listed for sites ',A,1X,A)
 1030 FORMAT (A,' I/O error',I6)
 1050 FORMAT ('Disk repeated in NETSP: ',A)
 1110 FORMAT ('Hosts ',A,1X,A,' found in ',A)
 1120 FORMAT ('No host name in ',A)
 1300 FORMAT ('NETSP entry used',I3,' times: ',A)
 1310 FORMAT ('Host ',A,' not mentioned in the DADEVS files.')
      END
      SUBROUTINE NXTSTR (KB, KBPLIM, KBP, ISTR, NCHAR)
C-----------------------------------------------------------------------
C   NXTSTR obtains a character value from a buffer - blank separators
C   Inputs:
C      KB      C*80     character buffer
C      KBPLIM  I        size of buffer
C      KBP     I        start position in KB
C   Outputs:
C      KBP     I        start position in KB next field
C      ISTR    C*(*)    packed string, blank filled
C      NCHAR   I        # characters (0 => no string found)
C-----------------------------------------------------------------------
      CHARACTER KB*80, ISTR*(*)
      INTEGER   KBPLIM, KBP, NCHAR
C
      INTEGER   JB, JJ, LMAX
C-----------------------------------------------------------------------
      NCHAR = 0
      LMAX = LEN (ISTR)
      ISTR = ' '
      IF (KBP.GT.KBPLIM) GO TO 999
C                                        skip leading blanks
 10   IF (KB(KBP:KBP).EQ.' ') THEN
         KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 999
         GO TO 10
         END IF
C                                        find end
 15   JB = KBP
 20   KBP = KBP + 1
         IF (KBP.GT.KBPLIM) GO TO 25
         IF (KB(KBP:KBP).EQ.' ') GO TO 25
         GO TO 20
C                                        got it
 25   NCHAR = KBP - JB
      KBP = KBP + 1
      JJ = MIN (NCHAR, LMAX)
      NCHAR = MIN (NCHAR, LMAX)
      IF (NCHAR.NE.0) ISTR(1:JJ) = KB(JB:JB+JJ-1)
C                                        make null string ok
      IF (NCHAR.EQ.0) NCHAR = 1
C
 999  RETURN
      END
