      SUBROUTINE PLINIT (IGTYPE, IVOL, ICNO, CATBLK, NP, RPARM, WDOTV,
     *   IGRCHN, ITVCHN, ITVORG, PLTBUF, IERR)
C-----------------------------------------------------------------------
C! initialize plotting commons
C# Utility Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1997, 2002, 2007
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
C   This routine will initialise plotting to a TV device or a plot
C   file. The PL file is opened if necessary and the catalog header
C   updated. The catalog header needs previously to have been read
C   and the status marked as 'WRIT'.
C   Input parameters:
C      IVOL     I       Disk volume number.
C      ICNO     I       Catalog slot number.
C      CATBLK   I(256)  Catalog header.
C      NP       I       Number of floating point words in parameter
C                       list received from AIPS.
C      RPARM    R(NP)   AIPS parameters.
C      WDOTV    L       True to use TV, else write to PL file.
C      IGRCHN   I       TV Grey channel to use (0 => 1)
C      ITVCHN   I       TV graphics channel to use (0 => 1)
C      ITVORG   I(2)    X,Y TV pixel to put plot origin (0=> centre)
C   Input/ output:
C      IGTYPE   I       Task specific plot file type: 1=misc.; else
C                       consult AIPSUB:AU8A.
C      PLTBUF   I(256)  Work Buffer
C   Output parameters:
C      IERR     I       Error code (0 => ok)
C   Output via common:
C      JPLVER     I       PL file version created.
C      GPHIND   I       in /GPHCOM/.
C      TVCORN   I(4)    Set to zeroes.
C      WPDOTV   L       Plot on TV device ?
C      GRCHN    I       Graphics channel.
C      IGFIND   I       Location in FTAB for graphics file.
C      IGLUN    I       Graphics LU number.
C      TVCHN    I       TV channel.
C      LPLNAM   C*48    Physical filename PL file.
C      IPLSTA   I       Plot file status (1=>open; 0=> closed)
C-----------------------------------------------------------------------
      LOGICAL   WDOTV
      INTEGER   CATBLK(256), PLTBUF(256), IGTYPE, IVOL, ICNO, NP,
     *   IGRCHN, ITVCHN, ITVORG(2), IERR
      REAL      RPARM(*)
C
      INTEGER   IGSIZE, IER
      LOGICAL   WTRUE
      INCLUDE 'INCS:DGPH.INC'
      INCLUDE 'INCS:DPLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IGSIZE /0/
      DATA WTRUE /.TRUE./
C-----------------------------------------------------------------------
C                                       Set common variables in DPLT
      WPDOTV = WDOTV
      TVCHN = ITVCHN
      GRCHN = IGRCHN
      CALL COPY (2, ITVORG, TVCORN)
C                                       Check range of plot type
      IF (IGTYPE.LT.1) IGTYPE = 1
C                                       Add plot file to header
      JPLVER = 0
      IF (.NOT.WDOTV) THEN
         CALL MADDEX ('PL', IVOL, ICNO, CATBLK, PLTBUF, WTRUE, 'READ',
     *      JPLVER, IERR)
         IF (IERR.NE.0) GO TO 999
         WRITE (MSGTXT,1020) JPLVER
         CALL MSGWRT (6)
         END IF
C                                       Construct physical filename
      CALL ZPHFIL ('PL', IVOL, ICNO, JPLVER, LPLNAM, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Open plot file
      CALL GINIT (IVOL, ICNO, LPLNAM, IGSIZE, IGTYPE, NP, RPARM, WDOTV,
     *   TVCHN, GRCHN, TVCORN, CATBLK, PLTBLK, IGLUN, IGFIND, IERR)
      IF (IERR.NE.0) THEN
         GPHIND = 0
         IPLSTA = 0
         IF (.NOT.WDOTV) CALL DELEXT ('PL', IVOL, ICNO, 'WRIT', CATBLK,
     *      PLTBUF, JPLVER, IER)
      ELSE
         IPLSTA = 1
         END IF
C                                       Exit
 999  RETURN
C-----------------------------------------------------------------------
1020  FORMAT ('Plot file version',I5,' created')
      END
