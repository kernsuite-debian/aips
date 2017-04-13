      SUBROUTINE SOUNDX (DISKIN, CNOIN, DOWANT, NUMSOU, SULIST, IERR)
C-----------------------------------------------------------------------
C! Compares source wanted list with INDEX table
C# EXT-util UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   SOUNDX: compares a source list with entries in the INDEX table to
C   Determine whether sources were observed with a requested Frequency
C   ID (FRQSEL).  SOUNDX removes sources from an already created list
C   or adds sources to a wanted source list.  SOUNDX does nothing
C   if all Frequency Ids are selected (FRQSEL<=0).
C   This program is designed to solve the problem of two identical
C   source names with different qualifiers.
C    Inputs:
C     DISKIN       I   Input Disk Number
C     CNOIN        I   Input Catalog number
C    Input/output:
C     DOWANT       L   TRUE if sources in list are wanted (not excluded)
C     NUMSOU       I   Number of sources wanted/unwanted
C     SULIST(?)    I   List of sources wanted
C    Output:
C     IERR         I   Error code, 0=>OK else TABIO error.
C                                 10=>buffer size error
C    Inputs from common:
C     FRQSEL       I   Frequency ID wanted
C-----------------------------------------------------------------------
      INTEGER DISKIN, CNOIN, NUMSOU, SULIST(*), IERR
      LOGICAL DOWANT
C-----------------------------------------------------------------------
      INCLUDE 'INCS:DSEL.INC'
      INTEGER INXLUN, NDXRNO, LOOP, I, NXSOUR, NXSUB, NXVS, NXVE, NXFQI,
     *   NXVER, SELIST(XSTBSZ), NINLST
      LOGICAL INLIST, WANTED
      REAL    NXTIME, NXDTIM
      INCLUDE 'INCS:DMSG.INC'
      DATA INXLUN /28/
C-----------------------------------------------------------------------
      IERR=0
C                                       If no frequencies selected, exit
      IF (FRQSEL.LE.0) GO TO 999
C                                       If no sources in list, exclude
      WANTED = DOWANT
      IF (NUMSOU.LE.0) WANTED = .FALSE.
C                                       If not enough buffer space
      IF (NUMSOU.GT.XSTBSZ) THEN
         MSGTXT = 'SCRATCH SOURCE LIST BUFFER SIZE EXCEEDED'
         CALL MSGWRT(5)
         NUMSOU = XSTBSZ
         IERR = 10
         END IF
C                                       If wanted init Search list
      IF (WANTED) THEN
         CALL FILL (NUMSOU, 0, SELIST)
C                                       Else copy unwanted list
      ELSE
         CALL COPY (NUMSOU, SULIST, SELIST)
         END IF
C                                       Init Output Count Source
      NINLST = 0
C                                       Initialize index table
      NXVER = 1
      CALL NDXINI ('READ', NXBUFF, DISKIN, CNOIN, NXVER, CATBLK, INXLUN,
     *   NDXRNO, NXKOLS, NXNUMV, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Get number of index records
      NINDEX = NXBUFF(5)
C                                       Loop thru index table
      DO 300 LOOP = 1,NINDEX
C                                       Read index table
         NDXRNO = LOOP
         CALL TABNDX ('READ', NXBUFF, NDXRNO, NXKOLS, NXNUMV, NXTIME,
     *      NXDTIM, NXSOUR, NXSUB, NXVS, NXVE, NXFQI, IERR)
         IF (IERR.GT.0) GO TO 999
C                                       For all sources in wanted list
         IF (WANTED) THEN
C                                       If source # is wanted + OK freq
C                                       Mark as a wanted source
            DO 100 I = 1,NUMSOU
               IF ((NXSOUR.EQ.SULIST(I)) .AND. ((FRQSEL.EQ.NXFQI) .OR.
     *            (FRQSEL.LE.0) .OR. (NXFQI.LE.0))) SELIST(I) = NXSOUR
 100           CONTINUE
C                                       Else an unwanted list
         ELSE
C                                       Assume source not in list
            INLIST = .FALSE.
C                                       If source # is in unwanted list
            IF (NUMSOU.GT.0) THEN
               DO 200 I = 1,NUMSOU
                  IF (NXSOUR.EQ.SELIST(I)) INLIST = .TRUE.
 200              CONTINUE
               END IF
C                                       If source # is in wanted list
C                                       Do not add again
            IF (NINLST.GT.0) THEN
               DO 250 I = 1,NINLST
                  IF (NXSOUR.EQ.SULIST(I)) INLIST = .TRUE.
 250              CONTINUE
               END IF
C                                       If freq id matched add to wanted
C                                       list
            IF ((.NOT.INLIST) .AND. ((FRQSEL.EQ.NXFQI) .OR.
     *         (FRQSEL.LE.0) .OR. (NXFQI.LE.0))) THEN
               NINLST = NINLST + 1
               IF (NINLST.GT.XSTBSZ) THEN
                  MSGTXT = 'SOUNDX: SOURCE LIST SIZE EXCEEDED'
                  CALL MSGWRT(5)
                  NINLST = XSTBSZ
                  IERR = 10
                  END IF
               SULIST(NINLST) = NXSOUR
               END IF
            END IF
 300     CONTINUE
C                                       Close  INDEX table
      CALL TABIO ('CLOS', 0, NDXRNO, NXBUFF, NXBUFF, IERR)
C                                       If started with a wanted list
      IF (WANTED) THEN
C                                       Re-Fill Wanted list
C                                       If in match list
         DO 400 I = 1,NUMSOU
            IF (SELIST(I).GT.0) THEN
               NINLST = NINLST + 1
               SULIST(NINLST) = SELIST(I)
               END IF
 400        CONTINUE
         END IF
C                                       Rest count
      NUMSOU = NINLST
C                                       Zero rest of list
      IF (NUMSOU.LT.XSTBSZ)
     *   CALL FILL (XSTBSZ-NUMSOU, 0, SULIST(NUMSOU+1))
C                                       If sources are left, use WANTED
      IF (NUMSOU.GE.0) DOWANT = .TRUE.
C
 999  RETURN
      END




