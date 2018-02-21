# ==============================================================================
# Functions for inspecting and managing apps for Cytoscape.
# ------------------------------------------------------------------------------
#' @title Disable App
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' disableApp()
#' }
#' @export
disableApp <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET(paste0('apps disable app="',app,'"'), base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Enable App
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' enableApp()
#' }
#' @export
enableApp <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET(paste0('apps enable app="',app,'"'), base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Get App Information
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getAppInformation()
#' }
#' @export
getAppInformation <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET(paste0('apps information app="',app,'"'), base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Install App
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' installApp()
#' }
#' @export
installApp <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET(paste0('apps install app="',app,'"'), base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title List Available Apps
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getAvailableApps()
#' }
#' @export
getAvailableApps <- function (base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET('apps list available', base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title List Disabled Apps
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getDisabledApps()
#' }
#' @export
getDisabledApps <- function (base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET('apps list disabled', base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title List Installed Apps
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getInstalledApps()
#' }
#' @export
getInstalledApps <- function (base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET('apps list installed', base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title List Uninstalled Apps
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getUninstalledApps()
#' }
#' @export
getUninstalledApps <- function (base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET('apps list uninstalled', base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title List Apps With Updates
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getAppUpdates()
#' }
#' @export
getAppUpdates <- function (base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET('apps list updates', base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Open App Store Page
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' openAppStore()
#' }
#' @export
openAppStore <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET(paste0('apps open appstore app="',app,'"'), base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title App Status
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getAppStatus()
#' }
#' @export
getAppStatus <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET(paste0('apps status app="',app,'"'), base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Uninstall App
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' uninstallApp()
#' }
#' @export
uninstallApp <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET(paste0('apps uninstall app="',app,'"'), base.url=base.url)
}

# ------------------------------------------------------------------------------
#' @title Update App
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url (optional) Ignore unless you need to specify a custom domain,
#' port or version to connect to the CyREST API. Default is http://localhost:1234
#' and the latest version of the CyREST API supported by this version of RCy3.
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' updateApp()
#' }
#' @export
updateApp <- function (app, base.url=.defaultBaseUrl){
    .verifySupportedVersions(1,3.7)
    commandsGET(paste0('apps update app="',app,'"'), base.url=base.url)
}

