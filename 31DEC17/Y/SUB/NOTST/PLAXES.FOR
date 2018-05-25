      SUBROUTINE PLAXES (BLC, TRC, XMIN, XMAX, YMIN, YMAX, JLTYPE,
     *   LXUNIT, LYUNIT, NABOVE, NBELOW, LABOVE, LBELOW, NSIZE, WSUBPL,
     *   IRET)
C-----------------------------------------------------------------------
C! plots axes and labels using info in plotting commons
C# Utility Plot
C-----------------------------------------------------------------------
C;  Copyright (C) 1997-1998, 2002-2003, 2007, 2014
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
C   Routine to plot and label axes for a line plot.
C   Input:
C      BLC     R(2)     X,Y of bottom left corner (in pixels)
C      TRC     R(2)     X,Y of top tight corner (in pixels)
C      XMIN    R        Minimum x-value
C      XMAX    R        Maximum x-value
C      YMIN    R        Minimum y-value
C      YMAX    R        Maximum y-value
C      JLTYPE  I        Labelling style (conforms to adverb LTYPE)
C      LXUNIT  C*20     Units to label x-axis
C      LYUNIT  C*20     Units to label y-axis
C      NABOVE  I        Number of title lines above plot (excl.
C                       plot version and date)
C      NBELOW  I        Number of title lines below plot (excl.
C                       plot version and date)
C      LABOVE  C(*)*80  Title lines above plot
C      LBELOW  C(*)*80  Title lines below plot
C      NSIZE   I        Declared dimension of LABOVE and LBELOW.
C      WSUBPL  L        True if subplot; must have defined the overall
C                       page size using BLC,TRC in an earlier call to
C                       PLAXES with WSUBPL false.
C   Output:
C      IRET    I        Termination status (0 => ok)
C-----------------------------------------------------------------------
      LOGICAL   WSUBPL
      INTEGER   JLTYPE, NABOVE, NBELOW, NSIZE, IRET
      CHARACTER LABOVE(NSIZE)*80, LBELOW(NSIZE)*80, LXUNIT*20,
     *   LYUNIT*20
      REAL      BLC(2), TRC(2), XMIN, XMAX, YMIN, YMAX
C
      INCLUDE 'INCS:DLOC.INC'
      INCLUDE 'INCS:DPLD.INC'
      INCLUDE 'INCS:DMSG.INC'
      LOGICAL   WERR, WFALSE, WAXES
      CHARACTER LTIME*8, LDATE*12, LTEXT*80
      REAL      XREF1, YREF1, XREF2, YREF2, CHOUT(4), XYRAT, XPOS, YPOS
      INTEGER   ILEFT, JLAB, IDEPTH(5), IERR, ID(3), IT(3), NCHAR, I
      DATA WFALSE /.FALSE./
C-----------------------------------------------------------------------
C                                       Initialisation
      IRET = 0
C                                       Only plot axes if non-zero
C                                       range.
      WAXES = ((XMIN.NE.XMAX) .AND. (YMIN.NE.YMAX))
C                                       Set up DLOC.INC
      LOCNUM = 1
      ROT(LOCNUM) = 0.0
      CORTYP(LOCNUM) = 0
      LABTYP(LOCNUM) = 0
      AXTYP(LOCNUM) = 0
      AXFUNC(1,LOCNUM) = 0
      AXFUNC(2,LOCNUM) = 0
      CPREF(1,LOCNUM) = ' '
      CPREF(2,LOCNUM) = ' '
      IF (LXUNIT.EQ.'TIME (HOURS)') LABTYP(LOCNUM) = 7
      IF (LYUNIT.EQ.'TIME (HOURS)') LABTYP(LOCNUM) = 70
C                                       Set BLC,TRC in DPLD.INC
      CALL RCOPY (2, BLC, PLBLC)
      CALL RCOPY (2, TRC, PLTRC)
C                                       Scale x-axis
      XREF1 = MAX (ABS (XMIN), ABS (XMAX))
      XREF1 = MAX (XREF1, ABS (XMAX-XMIN))
      XREF2 = XREF1
      IF (WAXES) THEN
         CALL METSCL (JLTYPE, XREF2, CPREF(1,LOCNUM), WERR)
         IF (WERR) THEN
            IRET = 1
            WRITE (MSGTXT,1020) 'X'
            CALL MSGWRT (8)
            WRITE (MSGTXT,1021) XMIN, XMAX
            GO TO 990
            END IF
         END IF
      IF (LXUNIT.EQ.'TIME (HOURS)') THEN
         XPFACT = 360.0
         CPREF(1,LOCNUM) = ' '
      ELSE IF (XREF1.NE.0.0) THEN
         XPFACT = XREF2 / XREF1
      ELSE
         XPFACT = 1.0
         END IF
      CTYP(1,LOCNUM) = LXUNIT
      RPLOC(1,LOCNUM) = BLC(1)
      RPVAL(1,LOCNUM) = XMIN * XPFACT
      AXINC(1,LOCNUM) = XPFACT * (XMAX - XMIN) / (TRC(1) - BLC(1))
C                                       Scale y-axis
      YREF1 = MAX (ABS (YMIN), ABS (YMAX))
      YREF1 = MAX (YREF1, ABS (YMAX-YMIN))
      YREF2 = YREF1
      IF (WAXES) THEN
         CALL METSCL (JLTYPE, YREF2, CPREF(2,LOCNUM), WERR)
         IF (WERR) THEN
            IRET = 1
            WRITE (MSGTXT,1020) 'Y'
            CALL MSGWRT (8)
            WRITE (MSGTXT,1021) YMIN, YMAX
            GO TO 990
            END IF
         END IF
      IF (LYUNIT.EQ.'TIME (HOURS)') THEN
         YPFACT = 360.0
         CPREF(2,LOCNUM) = ' '
      ELSE IF (YREF1.NE.0.0) THEN
         YPFACT = YREF2 / YREF1
      ELSE
         YPFACT = 1.0
         END IF
      CTYP(2,LOCNUM) = LYUNIT
      RPLOC(2,LOCNUM) = BLC(2)
      RPVAL(2,LOCNUM) = YMIN * YPFACT
      AXINC(2,LOCNUM) = YPFACT * (YMAX - YMIN) / (TRC(2) - BLC(2))
C                                       Set text borders
      CALL RFILL (4, 0.5, CHOUT)
      CALL CHNTIC (BLC, TRC, ILEFT)
      JLAB = MOD (ABS (JLTYPE), 100)
C                                       Comply with AIPS convention
C                                       for plot labelling (see
C                                       adverb LTYPE)
C                                       No tick labels y-axis
      IF ((JLAB.EQ.2).AND.(WAXES)) CHOUT(1) = 2.5
C                                       Full labels y-axis
      IF ((JLAB.GT.2).AND.(WAXES)) CHOUT(1) = ILEFT + 4.0
C                                       No tick labels x-axis
      IF ((JLAB.GT.1).AND.(WAXES)) CHOUT(2) = 2.0
C                                       Full labels x-axis
      IF ((JLAB.GT.2).AND.(WAXES)) CHOUT(2) = CHOUT(2) + 1.333
C                                       PL version/date line
      IF ((JLTYPE.GT.1).AND.(.NOT.WSUBPL)) CHOUT(4) = CHOUT(4) + 1.333
C                                       Plot titles
C                                       Add space for other title lines
      IF ((JLAB.GT.1).AND.(JLAB.LT.7)) THEN
         CHOUT(2) = CHOUT(2) + MAX (NBELOW, 0) * 1.333
         CHOUT(4) = CHOUT(4) + MAX (NABOVE, 0) * 1.333
         END IF
      IF (CHOUT(4).GT.0.5) CHOUT(4) = CHOUT(4) + 0.333
C                                       If call to open main plot page,
C                                       then init. for line drawing
      IF (.NOT.WSUBPL) THEN
         CALL FILL (5, 1, IDEPTH)
         XYRAT = 1.0
         CALL GINITL (BLC, TRC, XYRAT, CHOUT, IDEPTH, PLTBLK, IERR)
         IF (IERR.NE.0) THEN
            IRET = 2
            WRITE (MSGTXT,1040) IERR
            GO TO 990
            END IF
         END IF
C                                       line type 1
      CALL GLTYPE (1, PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Draw the axes box
      CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (TRC(1), TRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), TRC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 970
      CALL GVEC (BLC(1), BLC(2), PLTBLK, IERR)
      IF (IERR.NE.0) GO TO 970
C                                       Label the axes
      IF (WAXES) THEN
         CALL CLAB1 (BLC, TRC, CHOUT, JLTYPE, XYRAT, WFALSE, PLTBLK,
     *      IERR)
         IF (IERR.NE.0) THEN
            IRET = 3
            WRITE (MSGTXT,1100) IERR
            GO TO 990
            END IF
         END IF
C                                       Add titles
C                                       PL version; date/time
      XPOS = 0.0
      YPOS = CHOUT(4) - 1.333
C
      IF ((JLTYPE.GT.1) .AND. (.NOT.WSUBPL)) THEN
         CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 970
         CALL ZDATE (ID)
         CALL ZTIME (IT)
         CALL TIMDAT (IT, ID, LTIME, LDATE)
         WRITE (LTEXT,1150) JPLVER, LDATE, LTIME
         CALL REFRMT (LTEXT, '_', NCHAR)
         CALL GCHAR (NCHAR, 0, XPOS, YPOS, LTEXT, PLTBLK, IERR)
         IF (IERR.NE.0) GO TO 970
         YPOS = YPOS - 1.333
         END IF
C                                       Add titles above plot
      IF ((JLAB.GT.1) .AND. (JLAB.LT.7)) THEN
         DO 200 I = 1,NABOVE
            CALL GPOS (BLC(1), TRC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 970
            LTEXT = LABOVE(I)
            CALL CHTRIM (LTEXT, 80, LTEXT, NCHAR)
            CALL GCHAR (NCHAR, 0, XPOS, YPOS, LTEXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 970
            YPOS = YPOS - 1.333
 200        CONTINUE
C                                       Add titles below plot
         XPOS = 0.0
         YPOS = -CHOUT(2)
         DO 300 I = 1,NBELOW
            CALL GPOS (BLC(1), BLC(2), PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 970
            LTEXT = LBELOW(NBELOW - I+ 1)
            CALL CHTRIM (LTEXT, 80, LTEXT, NCHAR)
            CALL GCHAR (NCHAR, 0, XPOS, YPOS, LTEXT, PLTBLK, IERR)
            IF (IERR.NE.0) GO TO 970
            YPOS = YPOS + 1.333
 300        CONTINUE
         END IF
      GO TO 999
C                                       Error with plot internals
 970  WRITE (MSGTXT,1970) IERR
      IRET = 9
C                                       Error
 990  CALL MSGWRT (8)
C                                       Exit
 999  RETURN
C-----------------------------------------------------------------------
 1020 FORMAT ('PLAXES: ERROR SCALING UNITS OF ',A1,' AXIS')
 1021 FORMAT ('PLAXES: AXIS VALUES ARE',2(1PE14.5))
 1040 FORMAT ('PLAXES: ERROR',I3,' FROM GINITL')
 1100 FORMAT ('PLAXES: ERROR',I3,' FROM CLAB1')
 1150 FORMAT ('Plot file version',I4,'__created ',A12,A8)
 1970 FORMAT ('PLAXES: ERROR',I3,' FROM GPOS, GVEC OR GCHAR')
      END
