      LOGICAL FUNCTION SLCTD (ITEM, LIST, NITEMS, INCLD)
C-----------------------------------------------------------------------
C! Determines whether an item has been selected by the user
C# Utility
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
C   User selections (eg. antennas, sources and calibration sources) are
C   usually stored as a list of integer codes and a flag which indicates
C   whether the requested items in the list are to be included in or
C   excluded from the selection. Given a list of NITEMS requested items
C   and a flag, INCLD, which is true if the requested items are included
C   in the selection and false if the requested items are excluded from
C   the selection, return true if ITEM is a selected item or false if
C   ITEM is not selected. If NITEMS <= 0 then it is assumed that all
C   items are selected.
C   If INCLD is false, the item and the list are compared in absolute
C   value.
C   Inputs:
C     ITEM     I         Item to test.
C     LIST     I(*)      List of requested items.
C     NITEMS   I         Number of items in LIST.
C     INCLD    L         .TRUE. if the requested items are included in
C                        the selected, .FALSE. if the requested items
C                        are excluded from the selection.
C
C   Return value:
C     SLCTD    L         .TRUE. if ITEM is selected, .FALSE. if ITEM is
C                        not selected.
C-----------------------------------------------------------------------
      INTEGER   ITEM, LIST(*), NITEMS
      LOGICAL   INCLD
C
      INTEGER   I
      LOGICAL   INLIST
C-----------------------------------------------------------------------
      IF (NITEMS.LE.0) THEN
         SLCTD = .TRUE.
      ELSE
         INLIST = .FALSE.
         IF (INCLD) THEN
            DO 10 I = 1,NITEMS
               IF (ITEM.EQ.LIST(I)) INLIST = .TRUE.
 10            CONTINUE
         ELSE
            DO 20 I = 1,NITEMS
               IF (ABS(ITEM).EQ.ABS(LIST(I))) INLIST = .TRUE.
 20            CONTINUE
            END IF
C                                       INLIST is true if ITEM is one
C                                       of the first NITEMS entries in
C                                       LIST and false otherwise.
         SLCTD = INLIST.EQV.INCLD
C                                       INLIST.EQV.INCLD is equivalent
C                                       to (INLIST.AND.INCLD) .OR.
C                                       (.NOT.INLIST .AND. .NOT.INCLD).
         END IF
C
 999  RETURN
      END
