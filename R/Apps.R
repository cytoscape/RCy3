# ==============================================================================
# Functions for inspecting and managing apps for Cytoscape.
# ------------------------------------------------------------------------------
#' @title Disable App
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' disableApp()
#' }
#' @export
disableApp <- function (app, base.url=.defaultBaseUrl){
    commandsGET('apps disable app="',app,'"')
}

# ------------------------------------------------------------------------------
#' @title Enable App
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' enableApp()
#' }
#' @export
enableApp <- function (app, base.url=.defaultBaseUrl){
    commandsGET('apps enable app="',app,'"')
}

# ------------------------------------------------------------------------------
#' @title Get App Information
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getAppInformation()
#' }
#' @export
getAppInformation <- function (app, base.url=.defaultBaseUrl){
    commandsGET('apps information app="',app,'"')
}

# ------------------------------------------------------------------------------
#' @title Install App
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' installApp()
#' }
#' @export
installApp <- function (app, base.url=.defaultBaseUrl){
    commandsGET('apps install app="',app,'"')
}

# ------------------------------------------------------------------------------
#' @title List Available Apps
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getAvailableApps()
#' }
#' @export
getAvailableApps <- function (base.url=.defaultBaseUrl){
    commandsGET('apps list available')
}

# ------------------------------------------------------------------------------
#' @title List Disabled Apps
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getDisabledApps()
#' }
#' @export
getDisabledApps <- function (base.url=.defaultBaseUrl){
    commandsGET('apps list disabled')
}

# ------------------------------------------------------------------------------
#' @title List Installed Apps
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getInstalledApps()
#' }
#' @export
getInstalledApps <- function (base.url=.defaultBaseUrl){
    commandsGET('apps list installed')
}

# ------------------------------------------------------------------------------
#' @title List Uninstalled Apps
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getUninstalledApps()
#' }
#' @export
getUninstalledApps <- function (base.url=.defaultBaseUrl){
    commandsGET('apps list uninstalled')
}

# ------------------------------------------------------------------------------
#' @title List Apps With Updates
#'
#' @description FUNCTION_DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getAppUpdates()
#' }
#' @export
getAppUpdates <- function (base.url=.defaultBaseUrl){
    commandsGET('apps list updates')
}

# ------------------------------------------------------------------------------
#' @title Open App Store Page
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' openAppStore()
#' }
#' @export
openAppStore <- function (app, base.url=.defaultBaseUrl){
    commandsGET('apps open appstore app="',app,'"')
}

# ------------------------------------------------------------------------------
#' @title App Status
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' getAppStatus()
#' }
#' @export
getAppStatus <- function (app, base.url=.defaultBaseUrl){
    commandsGET('apps status app="',app,'"')
}

# ------------------------------------------------------------------------------
#' @title Uninstall App
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' uninstallApp()
#' }
#' @export
uninstallApp <- function (app, base.url=.defaultBaseUrl){
    commandsGET('apps uninstall app="',app,'"')
}

# ------------------------------------------------------------------------------
#' @title Update App
#'
#' @description FUNCTION_DESCRIPTION
#' @param app DESCRIPTION
#' @param base.url DESCRIPTION
#' @return RETURN_DESCRIPTION
#' @examples \donttest{
#' updateApp()
#' }
#' @export
updateApp <- function (app, base.url=.defaultBaseUrl){
    commandsGET('apps update app="',app,'"')
}

