      SUBROUTINE SLOCIN (INODIM)
C-----------------------------------------------------------------------
C! initialize location common for slice (on the TEK) model fitting
C# Slice Graphics
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2007, 2009, 2012
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
C   SLOCIN (slice LOCATI init) will initialize the location common
C   for the peculiar values needed for gaussian fitting of slice files.
C   Output;
C      INODIM   I   Number of dimensions (1 or 2) of fit. Always 1 now.
C   Commons output:
C      /MAPHDR/ filled with header associated with TK device
C      /LOCATI/ position common
C-----------------------------------------------------------------------
      INTEGER   INODIM
C
      CHARACTER TEXT(2)*80
      REAL      BLC(2), TRC(2), XBLC(7), XTRC(7), CH(4), RANGE(2),
     *   YGAP, FQFINC
      INTEGER   IDEPTH(5), IDROP(2), ITYPE, IX, IY, I, IRXTRA, IERR,
     *   INOSL, K, LABEL, NTEXT
      DOUBLE PRECISION FQFREQ
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DCAT.INC'
C-----------------------------------------------------------------------
C                                       Read tek image cat header.
      IX = 0
      IY = 0
      CALL TKCATL ('READ', IX, IY, CATBLK, IERR)
      IF ((IERR.GT.0) .AND. (IERR.NE.11)) GO TO 970
C                                       Test image type for slice.
      ITYPE = CATBLK(IIPLT)
      INODIM = 1
      IF (ITYPE.NE.5) GO TO 980
C                                       Reconstruct original values.
      LABEL = CATBLK(IIOTH)
      IDROP(1) = CATBLK(IIOTH+1)
      IDROP(2) = CATBLK(IIOTH+2)
      IRXTRA = IIOTH + 3
      XBLC(1) = CATR(IRXTRA)
      XBLC(2) = CATR(IRXTRA+1)
      XTRC(1) = CATR(IRXTRA+2)
      XTRC(2) = CATR(IRXTRA+3)
      XBLC(3) = CATR(IRXTRA+4)
      XTRC(3) = CATR(IRXTRA+5)
      IRXTRA = IRXTRA + 6
      IRXTRA = IRXTRA/2 + 1
      FQFREQ = CATD(IRXTRA)
      IRXTRA = 2*IRXTRA + 1
      FQFINC = CATR(IRXTRA)
      INOSL = CATBLK(IIWIN+2) - CATBLK(IIWIN) + IDROP(1) + IDROP(2) + 1
      RANGE(1) = CATR(IRRAN)
      RANGE(2) = CATR(IRRAN+1)
      BLC(2) = CATBLK(IIWIN+1)
      TRC(2) = CATBLK(IIWIN+3)
      K = 1
      DO 10 I = 4,7
         XBLC(I) = CATBLK(IIDEP+K)
         XTRC(I) = CATBLK(IIDEP+K)
         K = K + 1
 10      CONTINUE
C                                       Initialize location common.
      CALL SLBINI (IDROP, INOSL, RANGE, BLC, TRC, XBLC, XTRC, FQFREQ,
     *   FQFINC, IDEPTH, LABEL, YGAP, CH, TEXT, NTEXT)
      GO TO 999
C                                       Error conditions.
C                                       Image catalog error.
 970  WRITE (MSGTXT,1970)
      CALL MSGWRT (7)
      GO TO 999
C                                       Invalid plot type.
 980  WRITE (MSGTXT,1980) ITYPE
      CALL MSGWRT (6)
      GO TO 999
C
 999  RETURN
C-----------------------------------------------------------------------
 1970 FORMAT ('IMAGE CATALOG ERROR')
 1980 FORMAT ('INVALID PLOT TYPE',I6,' (ONLY SLICE ALLOWED SO FAR)')
      END
