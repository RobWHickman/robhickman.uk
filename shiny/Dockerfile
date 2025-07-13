FROM rocker/r-base:latest

# Install shiny package
RUN R -e "install.packages('shiny', repos='https://cran.rstudio.com/')"

# Copy app files to container
COPY . /app
WORKDIR /app

# Expose the port
EXPOSE 3000

# Run the start script
CMD ["Rscript", "start.R"]
