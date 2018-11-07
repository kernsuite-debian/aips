      LOGICAL FUNCTION IAMOK (IVOL, TYPE)
C-----------------------------------------------------------------------
C! decides if a disk file type is allowed for the user on a disk
C# IO-basic
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1996
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
C   Returns .TRUE. is a specific file type is allowed for the current
C   user on a specific disk.  Returns false if the type or disk or user
C   number are improper, or if DASSGN(n,IVOL) giving -1 for scratch disk
C   (no UV or MA files) or > 0 restricting the user numbers says
C   forbidden.
C   Inputs:
C      IVOL    I        Disk number (1-NVOL)
C      TYPE    C*2      File type (both characters upper case letters)
C   Input commons:
C      NLUSER  I        DMSG.INC : user number (1 - USELIM)
C      USELIM  I        DDCH.INC : max allowed user number
C      DASSGN  I(8,15)  DDCH.INC : disk assignments (#s,disk)
C                                  if #s = -1 => scratch
C                                  if > 0 => restrict to users listed
C   Output:
C      IAMOK   L        TRUE => do file creation/access, else don't
C-----------------------------------------------------------------------
      INTEGER   IVOL
      CHARACTER TYPE*2
C
      INTEGER   I
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IAMOK = .FALSE.
C                                       check ivol, user
      IF ((IVOL.LE.0) .OR. (IVOL.GT.NVOL)) GO TO 999
      IF ((NLUSER.LE.0) .OR. (NLUSER.GT.46655)) GO TO 999
      IF ((USELIM.GT.1) .AND. (NLUSER.GT.USELIM)) GO TO 999
C                                       check file type
      DO 10 I = 1,2
         IF (((TYPE(I:I).LT.'A') .OR. (TYPE(I:I).GT.'Z')) .AND.
     *      ((TYPE(I:I).LT.'a') .OR. (TYPE(I:I).GT.'z'))) GO TO 999
 10      CONTINUE
C                                       check allocation
C                                       scratch disk
      IF (DASSGN(1,IVOL).LT.0) THEN
         IF ((TYPE.NE.'MA') .AND. (TYPE.NE.'UV')) IAMOK = .TRUE.
C                                       assigned disk
      ELSE IF (DASSGN(1,IVOL).GT.0) THEN
         DO 25 I = 1,8
            IF (NLUSER.EQ.DASSGN(I,IVOL)) IAMOK = .TRUE.
 25         CONTINUE
         IF (NLUSER.EQ.1) IAMOK = .TRUE.
C                                       No limit on this disk
      ELSE
         IAMOK = .TRUE.
         END IF
C
 999  RETURN
      END
