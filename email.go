package main

import (
	"crypto/tls"
	"fmt"
	"log"
	"net/mail"
	"net/smtp"
)

func sendEmail(recipientAddress string, subject string, messageBody string) {
	if !siteDataLoaded {
		log.Panic("Outgoing email credentials have not been set. Cannot send message.")
	}

	from := mail.Address{Name: siteData.NoReplyAddressName, Address: siteData.NoReplyAddress}

	headers := make(map[string]string)
	headers["From"] = from.String()
	headers["To"] = recipientAddress
	headers["Subject"] = subject

	message := ""
	for headerName, headerValue := range headers {
		message += fmt.Sprintf("%s: %s\r\n", headerName, headerValue)
	}
	message += "\r\n" + messageBody

	mailAuth := smtp.PlainAuth("", siteData.NoReplyAddress, siteData.NoReplyPassword, siteData.Host)

	tlsConfig := &tls.Config{
		InsecureSkipVerify: true,
		ServerName:         siteData.Host,
	}

	tcpConnection, error := tls.Dial("tcp", siteData.Host+":"+siteData.Port, tlsConfig)
	panicOnError(error)

	smtpClient, error := smtp.NewClient(tcpConnection, siteData.Host)
	panicOnError(error)

	error = smtpClient.Auth(mailAuth)
	panicOnError(error)

	error = smtpClient.Mail(siteData.NoReplyAddress)
	panicOnError(error)

	error = smtpClient.Rcpt(recipientAddress)
	panicOnError(error)

	emailStream, error := smtpClient.Data()
	panicOnError(error)

	_, error = emailStream.Write([]byte(message))
	panicOnError(error)

	error = emailStream.Close()
	panicOnError(error)

	smtpClient.Quit()
}
