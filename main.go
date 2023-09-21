package main

import (
	"github.com/pulumi/pulumi-cloudflare/sdk/v5/go/cloudflare"
	"github.com/pulumi/pulumi/sdk/v3/go/pulumi"
)

func main() {
	pulumi.Run(func(ctx *pulumi.Context) error {
		Zone, err := cloudflare.NewZone(ctx, "autolife-robotics.tech", &cloudflare.ZoneArgs{
			AccountId: pulumi.String("2764ae0fd9a5cb92c9ac67708620e54c"),
			Plan:      pulumi.String("free"),
			Zone:      pulumi.String("autolife-robotics.tech"),
		}, pulumi.Protect(true))
		if err != nil {
			return err
		}

		_, err = cloudflare.NewRecord(ctx, "wild-card", &cloudflare.RecordArgs{
			ZoneId:  Zone.ID(),
			Name:    pulumi.String("*"),
			Value:   pulumi.String("43.156.66.157"),
			Type:    pulumi.String("A"),
			Ttl:     pulumi.Int(0),
			Proxied: pulumi.Bool(true),
		})
		if err != nil {
			return err
		}
		return nil
	})
}
