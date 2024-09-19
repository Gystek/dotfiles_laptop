#include <stddef.h>
#include <stdio.h>
#include <time.h>
#include <math.h>

#define GEPOCH (1721425.5)

int
ash_leap (int y)
{
  return (y % 4 == 0 && (y % 128 == 0 || y % 64 != 0));
}

int gregorian_leap (int y)
{
  return (!(y % 4) && (!(y % 400) || y % 100));
}

int
gregorian_jd (int d, int m, int y)
{
  int jd = GEPOCH - 1;

  jd += 365 * (y - 1);
  jd += floor ((y - 1) / 4);
  jd -= floor ((y - 1) / 100);
  jd += floor ((y - 1) / 400);

  jd += floor ((367 * m - 362) / 12);

  if (m > 2)
    jd -= gregorian_leap(y) ? 1 : 2;

  jd += d;

  return jd;
}

static int
jd_ash (float jd, int *d, int *m, int *y, int *dow)
{
  int leap, year, month, delta;
  int hepoc = gregorian_jd (18, 3, 622);

  *d = 1;
  *m = 1;
  *y = 1;

  delta = jd - hepoc;

  *dow = 2; /* Thursday */
  *dow += (int)delta;
  *dow %= 7;

  year = 365;
  leap = 0;
  month = 31;

  while (delta >= year)
    {
      delta -= year;
      *y += 1;

      leap = (!(*y % 4) && (!(*y % 128) || *y % 64));
      year = leap ? 366 : 365;
    }

    
  while (delta >= month)
    {
      delta -= month;
      *m += 1;

      switch (*m)
	{
	case 1:
	case 2:
	case 3:
	case 4:
	case 5:
	case 6:
	  month = 31;
	  break;
	case 7:
	case 8:
	case 9:
	case 10:
	case 11:
	  month = 30;
	  break;
	case 12:
	  month = leap ? 30 : 29;
	  break;
	}
    }

  *d += (int)delta;

  return 0;
}

int
main (void)
{
  const char *days[7] = {
#ifdef _POLSKI
#ifndef _SHORT_DAYS
    "Sobota",
    "Niedziela",
    "Poniedziałek",
    "Wtorek",
    "Środa",
    "Czwartek",
    "Dżumuha"
#else
    "Sob.",
    "Niedź.",
    "Pon.",
    "Wt.",
    "Śr.",
    "Czw.",
    "Dżu."
#endif /* _SHORT_DAYS */
#else
#ifndef _SHORT_DAYS
    "Samedi",
    "Primidi",
    "Duodi",
    "Tridi",
    "Tétradi",
    "Pentadi",
    "Joumouha"
#else
    "Sam",
    "Pri",
    "Duo",
    "Tri",
    "Tét",
    "Pen",
    "Jou"
#endif /* _SHORT_DAYS */
#endif /* _POLSKI */
  };
  
  const char *months[12] = {
#ifdef _POLSKI
/* Wykluczenie politeistycznych nazw miesięcy */
#ifndef _SHORT_MONTHS
    "Brzeźnia",
    "Kwietnia",
    "Trawnia",
    "Czerwca",
    "Lipca",
    "Sierpnia",
    "Września",
    "Października",
    "Listopada",
    "Grudnia",
    "Stycznia",
    "Lutego"
#else
    "Traw.",
    "Czerw.",
    "Lip.",
    "Sierp.",
    "Wrześ.",
    "Paźdź.",
    "List.",
    "Grudź.",
    "Stycz.",
    "Lut.",
    "Brzeź.",
    "Kwiec."
#endif /* _SHORT_MONTHS */
#else
#ifndef _SHORT_MONTHS
    "Germinal",
    "Floréal",
    "Prairial",
    "Messidor",
    "Thermidor",
    "Fructidor",
    "Vendémiaire",
    "Brumaire",
    "Frimaire",
    "Nivôse",
    "Pluviôse",
    "Ventôse"
#else
    "Germ",
    "Flor",
    "Prai",
    "Mess",
    "Ther",
    "Fruc",
    "Vend",
    "Brum",
    "Frim",
    "Niv",
    "Pluv",
    "Vent"
#endif /* _SHORT_MONTHS */
#endif /* _POLSKI */
  };

  static int gy, gm, gd, y = 0, m = 0, d = 0, dow;
  int jd;

  time_t t;
  struct tm tm;

  t = time (NULL);
  tm = *localtime(&t);

  gy = tm.tm_year + 1900;
  gm = tm.tm_mon + 1;
  gd = tm.tm_mday;

  jd = gregorian_jd (gd, gm, gy);

  jd_ash (jd, &d, &m, &y, &dow);

  printf ("%s %d %s %d\n", days[dow], d, months[m], y);

  return 0;
}
