package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"

	"github.com/julienschmidt/httprouter"
	"github.com/stripe/stripe-go"
	"github.com/stripe/stripe-go/charge"
)

type SuccessMessage struct {
	Success bool   `json:"success"`
	Message string `json:"message"`
}

func (successMessage *SuccessMessage) SetMessage(success bool, message string) {
	successMessage.Success = success
	successMessage.Message = message
}

var (
	cuffsAndPrices = map[string]uint64{
		"The Guy Magnet Hex":           500,
		"The 304 - Stainless Hex":      1000,
		"Steel Serrated Locking Hex":   800,
		"Steel Self Locking Black Hex": 800,
	}
)

func newOrderSubmission(responseWriter http.ResponseWriter, request *http.Request, requestParameters httprouter.Params) {
	successMessage := validNewOrderPostVariables(request)
	if !successMessage.Success {
		json.NewEncoder(responseWriter).Encode(successMessage)
		return
	}
	stripeToken := request.PostFormValue("stripe-token")
	costInCents := cuffsAndPrices[request.PostFormValue("which-cuff")]
	if siteData.LiveOrDev == "live" {
		stripe.Key = siteData.StripeLiveSecretKey
	} else {
		stripe.Key = siteData.StripeTestSecretKey
	}
	productDescription := request.PostFormValue("which-cuff") + ", Size: " + request.PostFormValue("slot-size") + ", Ear: " + request.PostFormValue("which-ear") + ", Quantity: " + request.PostFormValue("quantity")
	chargeParams := &stripe.ChargeParams{
		Amount:   costInCents,
		Currency: "usd",
		Desc:     productDescription,
	}
	chargeParams.SetSource(stripeToken)
	chargeResults, error := charge.New(chargeParams)
	if error != nil {
		successMessage.SetMessage(false, error.Error())
		json.NewEncoder(responseWriter).Encode(successMessage)
		return
	}
	json.NewEncoder(responseWriter).Encode(successMessage)
	sendOrderSuccessEmail(request, chargeResults)
}

func validNewOrderPostVariables(request *http.Request) SuccessMessage {
	successMessage := SuccessMessage{}
	if request.PostFormValue("stripe-token") == "" {
		successMessage.Message += "Somehow the payment did not go through. "
	}
	if request.PostFormValue("which-cuff") == "" {
		successMessage.Message += "The which-cuff data was not populated. "
	}
	if request.PostFormValue("slot-size") == "" {
		successMessage.Message += "The slot-size data was not populated. "
	}
	if request.PostFormValue("which-ear") == "" {
		successMessage.Message += "The which-ear data was not populated. "
	}
	if request.PostFormValue("quantity") == "" {
		successMessage.Message += "The quantity data was not populated. "
	}
	if successMessage.Message == "" {
		successMessage.Success = true
		debug("here")
	}
	return successMessage
}

func sendOrderSuccessEmail(request *http.Request, chargeResults *stripe.Charge) SuccessMessage {
	successMessage := SuccessMessage{Success: true}
	responseEmailTemplate, error := ioutil.ReadFile("email-templates/new-order.txt")
	if error != nil {
		successMessage.SetMessage(false, "Unable to open email template file.")
		return successMessage
	}
	message := searchReplaceResponseEmailTemplate(request, chargeResults, string(responseEmailTemplate))
	sendEmail(request.PostFormValue("customer-email"), "Receipt From hexcuff.com - Order #"+chargeResults.ID, message)
	sendEmail(siteData.ReplyAddress, "Receipt From hexcuff.com - Order #"+chargeResults.ID, message)
	return successMessage
}

func searchReplaceResponseEmailTemplate(request *http.Request, chargeResults *stripe.Charge, responseEmailTemplate string) (message string) {
	fmt.Printf("%+v\n", chargeResults)
	message = responseEmailTemplate
	message = searchReplaceFromForm(request, message, "CUSTOMER-NAME", "shipping-name")
	message = searchReplaceFromForm(request, message, "QUANTITY", "quantity")
	message = searchReplaceFromForm(request, message, "SLOT-SIZE", "slot-size")
	message = searchReplaceFromForm(request, message, "WHICH-CUFF", "which-cuff")
	message = searchReplaceFromForm(request, message, "WHICH-EAR", "which-ear")

	shippingAddress := request.PostFormValue("shipping-address-line-1")
	shippingAddressLine2 := request.PostFormValue("shipping-address-line-2")
	if shippingAddressLine2 != "" {
		shippingAddress += "\n" + shippingAddressLine2
	}
	message = strings.Replace(message, "SHIPPING-ADDRESS", shippingAddress, -1)

	message = searchReplaceFromForm(request, message, "SHIPPING-CITY", "shipping-city")
	message = searchReplaceFromForm(request, message, "SHIPPING-STATE", "shipping-state")
	message = searchReplaceFromForm(request, message, "SHIPPING-ZIP", "shipping-zip")
	orderTotal := centsToHumanReadableDollars(cuffsAndPrices[request.PostFormValue("which-cuff")])
	message = strings.Replace(message, "ORDER-TOTAL", orderTotal, -1)
	fmt.Println(message)
	return
}

func searchReplaceFromForm(request *http.Request, preSearchReplace string, search string, replace string) string {
	postSearchReplace := strings.Replace(preSearchReplace, search, request.PostFormValue(replace), -1)
	return postSearchReplace
}

func centsToHumanReadableDollars(cents uint64) string {
	dollars := float64(cents) / 100
	return fmt.Sprintf("%5.2f", dollars)
}
