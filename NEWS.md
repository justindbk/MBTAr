## Release notes MBTAr version 2.0.0

This new version provides a major update.

The MBTA's performance API went through major changes that forced the elimination of many functions of this package having to do with basic information that could be queried about the system (e.g., stops, routes, alerts). Some of these functions were moved over to the GTFS-compatible V3 API, which has quite different formatting from the previous V2 performance API. Several of the functions that relied on those API calls are now deprecated, though for crucial ones there is now a version that queries the new V3 API instead of the Performance API.
