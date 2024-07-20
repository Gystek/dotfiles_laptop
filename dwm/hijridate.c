/* gcc %s -O2 -lm -Wextra -Wall -Wwrite-strings -o hijridate */
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <time.h>

#define GEPOCH (1721425.5)
#define IEPOCH (1948439.5)

int
leap(y)
     int y;
{
  if (!(y % 4))
    if (!(y % 400) || y % 100)
      return 1;
  return 0;
}

int
hijri_julian(y, m, d)
     int y, m, d;
{
  int jd;

  jd = d;
  jd += (int)ceil (29.5 * (m - 1));
  jd += 354 * y;
  jd += floor ((3 + 11 * y) / 30);
  jd += IEPOCH;
  jd -= 354 + 1;

  return jd;
}

int
main (void)
{
  const char *is_months[12] = {
#ifndef __ASCII_MONTHS
    "Ṣafar",
      "Rabīʿ al-Awwal",
      "Rabīʿ ath-Thānī",
      "Jumādā al-ʾŪlā",
      "Jumādā ath-Thāniyah",
      "Rajab",
      "Shaʿbān",
      "Ramaḍān",
      "Shawwāl",
      "Dhū al-Qaʿdah",
      "Dhū al-Ḥijjah"
#else
      "Safar",
      "Rabi' I",
      "Rabi' II",
      "Jumada I",
      "Jumada II",
      "Rajab",
      "Sha'ban",
      "Ramadan",
      "Shawwal",
      "Dhu al-Qa'dah",
      "Dhu al-Hijjah"
#endif
  };

  int gy, gm, gd;
  int iy, im, id;
  double jd, _c;
  
  time_t t;
  struct tm tm;

  t  = time (NULL);
  tm = *localtime(&t);

  gy = tm.tm_year + 1900;
  gm = tm.tm_mon + 1;
  gd = tm.tm_mday;

  jd = GEPOCH - 1;
  jd += 365 * (gy - 1);
  
  jd += floor ((gy - 1) / 4);
  jd -= floor ((gy - 1) / 100);
  jd += floor ((gy - 1) / 400);

  jd += floor ((367 * gm - 362) / 12);

  if (gm > 2)
    jd -= leap(gy) ? 1 : 2;
  
  jd += gd;

  jd = floor (jd);
  jd += 0.5;

  _c = jd - IEPOCH;
  _c *= 30;
  _c += 10646;
  _c /= 10631;
  iy = (int)_c;

  _c = hijri_julian(iy, 1, 1);

  _c += 29;
  _c = jd - _c;
  _c /= 29.5;
  _c = _c > 11 ? 12 : _c + 1;
  im = (int)_c;

  _c = jd + 1;
  _c -= hijri_julian(iy, im, 1);
  id = (int)_c;

  printf("%d %s %d\n", id - 29, is_months[im - 1], iy);

  return 0;
}
