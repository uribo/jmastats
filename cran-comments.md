## Test environments

* macOS Ventura (13.4) install, R 4.3.1 (local)
* win-builder (devel and release)
* Rhub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub: Debian Linux, R-release, GCC

## R CMD check results

0 errors | 0 warnings | 3 note

* This is a new release (re-submit).
* Remove `VignetteBuilder` filed in DESCRIPTION.
* Removed functions and example handling of the user's home file space.
* Changed to execute all examples except those that take more than 5 seconds to get execution results due to http requests.
* Code that will complete within 5 seconds is executed in example, and code that does not take less than 5 seconds is wrapped in `\donttest{}`.
* checking data for non-ASCII characters ... NOTE
  Note: found 16726 marked UTF-8 strings
* checking CRAN incoming feasibility ... [18s] NOTE

```
Maintainer: 'Shinya Uryu <suika1127@gmail.com>'
   
   New submission
   
   Possibly misspelled words in DESCRIPTION:
     JMA (5:62)

Found the following (possibly) invalid URLs:
     URL: https://xml.kishou.go.jp
       From: man/read_kishou_feed.Rd
       Status: Error
       Message: libcurl error code 35:
         	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed). More detail may be available in the Windows System event log.
```

* The possible misspellings are in fact all correct. Indicates the abbreviation of the organization's name.
* I have checked the URL is correct.
