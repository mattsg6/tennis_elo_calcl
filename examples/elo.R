files = list.files(pattern="atp_matches_[^_]*.csv")

matches_raw= do.call("rbind", lapply(files, function(x) read.csv(x, sep=",")))
matches <- matches_raw[c("winner_name","loser_name","tourney_level","tourney_date","match_num")]
firstDate <- as.Date("1900-01-01")
matches$tourney_date <- as.Date(as.character(matches$tourney_date),format='%Y%m%d', origin = "1900/01/01")

matches <- matches[order(matches$tourney_date,matches$match_num),]

# Run computeElo for elo results in an environment indexed by player names
computeElo <- function() {
    playersToElo <- new.env(hash=TRUE)
    apply(matches,1,computeEloByRow)
    return(playersToElo)
}

computeEloByRow <- function(row) {
    updateElo(playersToElo, row[1], row[2], row[1], row[3],row[4])
    return(0)
}

updateElo <- function (plToElo, playerA, playerB, winner, level, matchDate) {
    rA <- tail(plToElo[[playerA]]$ranking,n=1)
    rB <- tail(plToElo[[playerB]]$ranking,n=1)

    if(is.null(rA)) {
        plToElo[[playerA]] <- data.frame(ranking=1500, date=firstDate)
        rA <- 1500
    }
    if(is.null(rB)) {
        plToElo[[playerB]] <- data.frame(ranking=1500, date=firstDate)
        rB <- 1500
    }
        
    eA <- 1 / (1 + 10 ^ ((rB - rA)/400))
    eB <- 1 / (1 + 10 ^ ((rA - rB)/400))
    
    if (winner==playerA) {
        sA <- 1
        sB <- 0
    } else {
        sA <- 0
        sB <- 1
    }

    kA <- 250/((length(plToElo[[playerA]])+5)^0.4)
    kB <- 250/((length(plToElo[[playerB]])+5)^0.4)
    k <- ifelse(level == "G", 1.2, 1)

    rA_new <- rA + (k*kA) * (sA-eA)
    rB_new <- rB + (k*kB) * (sB-eB)

    plToElo[[playerA]] <- rbind(plToElo[[playerA]],data.frame(ranking=rA_new, date=matchDate))
    plToElo[[playerB]] <- rbind(plToElo[[playerB]],data.frame(ranking=rB_new, date=matchDate))
}


